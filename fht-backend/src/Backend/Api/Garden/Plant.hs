{-# LANGUAGE
    TypeApplications
  , TypeOperators
  , RankNTypes 
#-}
module Backend.Api.Garden.Plant
  ( module Api.Garden.Plant
  , plantApiServerT
  , plantApiServer
  , plantApiApplication
  )
where

import qualified Data.Map                      as M
import qualified Data.Text                     as T

import           Polysemy
import           Polysemy.Reader

import "prelude-polysemy" Prelude.Polysemy.ID

import "dbstorage-polysemy" Polysemy.Database
import "dbstorage-polysemy" Database.Storage

import           Servant.API
import           Servant.Server

import           Backend.Garden.Plant.Logs
import           Backend.Garden.Plant
import           Backend.Garden.Plant.Types     ( PlantStorageErr(..) )

import "fht-data" Data.Garden.Plant
import "fht-api" Api.Garden.Plant

import "wai-cors" Network.Wai.Middleware.Cors

plantApiServerT
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => ServerT PlantApi (Sem r)
plantApiServerT =
  searchByName :<|> addPlant :<|> updatePlant :<|> (addLogs :<|> deletePlant)

updatePlant
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => Plant
  -> Sem r FullPlantData
updatePlant = dbUpdate . UpdatePlant >=> getPlant . headMay

addPlant
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => Plant
  -> Sem r FullPlantData
addPlant = dbUpdate . AddPlant >=> getPlant . headMay

getPlant
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => Maybe PlantId
  -> Sem r FullPlantData
getPlant mid =
  selectByIds mid
    >>= populatePlantData
    .   fmap _unStoredPlant
    .   headMay
    .   M.elems
    >>= maybe (notFound mid) pure
 where
  notFound =
    throwKnownError . StorageError . mappend "Unable to get plant: " . show

searchByName
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger , Embed IO] r
  => Maybe Text
  -> Sem r [FullPlantData]
searchByName = \case
  Nothing -> getAll
  (Just (T.strip -> name))
    | T.null name -> getAll
    | otherwise   -> dbSelect (SearchByName name) >>= populate
 where
  populate = populatePlantData . fmap _unStoredPlant . M.elems
  getAll   = dbSelect GetAllPlants >>= populate

addLogs
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => Maybe PlantId
  -> [MaintenanceLog]
  -> Sem r FullPlantData
addLogs mId ls = case mId of
  Just id -> dbUpdate (AddLogs id ls) >>= getPlant . headMay . fmap _slPlantId
  Nothing -> throwKnownError . StorageError $ "No ID in request!"

deletePlant
  :: forall r
   . Members '[ID , Error KnownError , Transaction , Reader Logger] r
  => Maybe PlantId
  -> Sem r NoContent
deletePlant mid = maybe noId (dbUpdate . DeletePlant) mid $> NoContent
  where noId = throwKnownError NoIdSupplied

plantApiServer
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Server PlantApi
plantApiServer natTrans =
  hoistServer (Proxy @PlantApi) natTrans (plantApiServerT @r)

plantApiApplication
  :: forall r
   . Members
       '[ID , Error KnownError , Transaction , Reader Logger , Embed IO]
       r
  => (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Application
plantApiApplication natTrans =
  serve (Proxy @PlantApi) (plantApiServer natTrans)
    -- & addHeaders [("access-control-allow-origin", "*")]
    & cors (const . Just $ corsPolicy)
 where
  corsPolicy = simpleCorsResourcePolicy
    { corsOrigins        = Just (["http://localhost:3003"], True)
    , corsMethods        = ["GET", "PUT", "POST", "DELETE", "OPTIONS"]
    , corsRequestHeaders = [ hAuthorization
                           , hContentType
                           , hContentEncoding
                           , hContentLength
                           ]
    , corsExposedHeaders = Just [hServer]
    }
