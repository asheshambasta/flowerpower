{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
{-|
Module: Api.Garden.Plant
Description: Plant API.

Endpoints and client functions for the plant API.

-}
module Api.Garden.Plant
  ( PlantApi
  , PerIdApi
  -- * Client 
  , CreatePlant
  , UpdatePlant
  , ListPlants
  , AddLog
  , DeletePlant
  , plantApiClient
  -- * Re-exports
  , BaseUrl(..)
  , Scheme(..)
  )
where

import qualified Reflex.Dom                    as RD
import "servant-reflex" Servant.Reflex
import           Protolude
import qualified "fht-data" Data.Garden.Plant  as Plant
import "servant" Servant.API
import           Api.Version                    ( ApiV1 )

type PlantIdQueryParam = QueryParam "plant_id" Plant.PlantId

-- | Api for top level actions on plants. 
type PlantApi = ApiV1 :> "plants" :> ( QueryParam "name" Text :> Get '[JSON] [Plant.Plant]
                                     -- add a new plant 
                                       :<|> ReqBody '[JSON] Plant.Plant :> Post '[JSON] Plant.Plant
                                     -- update an existing plant.
                                       :<|> ReqBody '[JSON] Plant.Plant :> Put '[JSON] Plant.Plant
                                       :<|> PerIdApi
                                     )

-- | Per plant api.
type PerIdApi = PlantIdQueryParam :> ReqBody '[JSON] [Plant.MaintenanceLog] :> Put '[JSON] Plant.MaintenanceLog
                 :<|> PlantIdQueryParam :> Delete '[JSON] Plant.Plant

type ListPlants t m
  =  RD.Dynamic t (QParam Text)
  -> RD.Event t ()
  -> m (RD.Event t (ReqResult () [Plant.Plant]))

type CreatePlant t m
  =  RD.Dynamic t (Either Text Plant.Plant)
  -> RD.Event t ()
  -> m (RD.Event t (ReqResult () Plant.Plant))
type UpdatePlant t m = CreatePlant t m

type AddLog t m
  =  RD.Dynamic t (QParam Plant.PlantId)
  -> RD.Dynamic t (Either Text [Plant.MaintenanceLog])
  -> RD.Event t ()
  -> m (RD.Event t (ReqResult () Plant.MaintenanceLog))

type DeletePlant t m
  =  RD.Dynamic t (QParam Plant.PlantId)
  -> RD.Event t ()
  -> m (RD.Event t (ReqResult () Plant.Plant))

-- | Convenience for creating client functions for some t and m 
plantApiClient
  :: forall t m
   . RD.MonadWidget t m
  => RD.Dynamic t BaseUrl
  -> ListPlants t m
     :<|> CreatePlant t m
     :<|> UpdatePlant t m
     :<|> (AddLog t m :<|> DeletePlant t m)
plantApiClient = client (Proxy @PlantApi) (Proxy @m) (Proxy @())
