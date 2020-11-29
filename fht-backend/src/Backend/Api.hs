{-# LANGUAGE
    TypeApplications
  , TypeOperators
  , RankNTypes 
#-}
module Backend.Api
  ( FhtBackendApi
  , fhtBackendApiApplication
  )
where

import           Polysemy
import           Polysemy.Reader

import "prelude-polysemy" Prelude.Polysemy.ID

import "dbstorage-polysemy" Polysemy.Database

import           Servant.API
import           Servant.Server.StaticFiles
import           Servant.Server

import           Backend.Runtime                ( StaticFilesDir(..) )

import "this"    Backend.Api.Garden.Plant

import "wai-cors" Network.Wai.Middleware.Cors
import "wai-app-static" WaiAppStatic.Types      ( unsafeToPiece )
import "wai-app-static" Network.Wai.Application.Static
                                                ( defaultWebAppSettings
                                                , ssIndices
                                                )

type FhtBackendApi = PlantApi :<|> Raw

fhtBackendApiServerT
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => StaticFilesDir
  -> ServerT FhtBackendApi (Sem r)
fhtBackendApiServerT (StaticFilesDir staticDir) =
  plantApiServerT :<|> serveDirectoryWith
    (settings { ssIndices = [unsafeToPiece "index.html"] }) -- serveDirectoryWebApp staticDir
  where settings = defaultWebAppSettings staticDir

fhtBackendApiServer
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => StaticFilesDir
  -> (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Server FhtBackendApi
fhtBackendApiServer staticDir natTrans = hoistServer
  (Proxy @FhtBackendApi)
  natTrans
  (fhtBackendApiServerT @r staticDir)

fhtBackendApiApplication
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => StaticFilesDir
  -> [Origin]
  -> (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Application
fhtBackendApiApplication staticDir origins natTrans =
  serve (Proxy @FhtBackendApi) (fhtBackendApiServer staticDir natTrans)
    -- & addHeaders [("access-control-allow-origin", "*")]
    & cors (const . Just $ corsPolicy)
 where
  corsPolicy = simpleCorsResourcePolicy
    { corsOrigins        = Just (origins, True)
    , corsMethods        = ["GET", "PUT", "POST", "DELETE", "OPTIONS"]
    , corsRequestHeaders = [ hAuthorization
                           , hContentType
                           , hContentEncoding
                           , hContentLength
                           ]
    , corsExposedHeaders = Just [hServer]
    }
