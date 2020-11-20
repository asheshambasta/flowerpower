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
import           Polysemy.Error

import "dbstorage-polysemy" Polysemy.Database
import "dbstorage-polysemy" Database.Storage

import           Servant.API
import           Servant.Server
import           Backend.Garden.Plant
import           Backend.Garden.Plant.Types     ( PlantStorageErr(..) )


import "this"    Backend.Runtime
import "this"    Backend.Garden.Plant

import "fht-data" Data.Garden.Plant
import "fht-api" Api.Garden.Plant

import           Network.Wai.Middleware.AddHeaders

plantApiServerT
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger] r
  => ServerT PlantApi (Sem r)
plantApiServerT = searchByName :<|> addPlant :<|> undefined :<|> undefined

addPlant
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger] r
  => Plant
  -> Sem r FullPlantData
addPlant = generateId >=> dbUpdate . AddPlant >=> getPlant . headMay
 where
  -- snowflake based id gen. 
  generateId = pure . identity
  getPlant mid =
    selectByIds mid
      >>= populatePlantData
      .   fmap _unStoredPlant
      .   headMay
      .   M.elems
      >>= maybe (notFound mid) pure
  notFound =
    throwKnownError
      . StorageError
      . mappend "Unable to get just stored plant:"
      . show

searchByName
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger] r
  => Maybe Text
  -> Sem r [FullPlantData]
searchByName = \case
  Nothing -> dbSelect GetAllPlants >>= populate
  (Just name) | T.null name -> dbSelect GetAllPlants >>= populate
              | otherwise   -> dbSelect (SearchByName name) >>= populate
  where populate = populatePlantData . fmap _unStoredPlant . M.elems

plantApiServer
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger] r
  => (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Server PlantApi
plantApiServer natTrans =
  hoistServer (Proxy @PlantApi) natTrans (plantApiServerT @r)

plantApiApplication
  :: forall r
   . Members '[Error KnownError , Transaction , Reader Logger] r
  => (forall r' a . (r' ~ r) => Sem r a -> Handler a)
  -> Application
plantApiApplication natTrans =
  serve (Proxy @PlantApi) (plantApiServer natTrans)
    & addHeaders [("access-control-allow-origin", "*")]
