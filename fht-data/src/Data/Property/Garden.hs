{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Property.Garden
  ( Garden'(..)
  , Garden
  , gId
  , gType
  , gName
  , gDesc
  , gShape
  , GardenType(..)
  , GardenId'(..)
  , GardenId
  , unGardenId
  , GardenShape'(..)
  , unGardenShape
  , GardenShape
  , pGarden
  , pGardenId
  , pGardenShape
  , GardenPlant'(..)
  , GardenIdPlantId
  , gpGarden
  , gpPlant
  , gpCoord
  , module Coord
  , module Plant
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Coordinate               as Coord
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Data.Property.Garden.Plant    as Plant
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

-- | A garden: a garden can belong to exactly one property.
--
-- A garden can have a shape: described by cartesian coordinates. 
--
-- Currently, just a flat shape of the garden is supported: in the future, we'd like to have multiple layers.
data Garden' id gtype name desc shape = Garden
  { _gId    :: id -- ^ ID 
  , _gType  :: gtype -- ^ Type of the garden 
  , _gName  :: name -- ^ Name of the garden 
  , _gDesc  :: desc -- ^ Optional description
  , _gShape :: shape -- ^ The shape of the garden.
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Garden = Garden' GardenId GardenType Text (Maybe Text) GardenShape

data GardenType = Interior | Exterior
                deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Unique ID of the garden
newtype GardenId' id = GardenId { _unGardenId :: id }
                   deriving (Eq, Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData , Ord, Num, Real, Enum, Integral) via id

type GardenId = GardenId' Int64

-- | The location of a plant in a garden.
data GardenPlant' gid pid coord = GardenPlant
  { _gpGarden :: gid -- ^ Garden ID 
  , _gpPlant  :: pid -- ^ Plant ID 
  , _gpCoord  :: coord -- ^ Coordinate of the plant in the garden 
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type GardenIdPlantId = GardenPlant' GardenId Plant.PlantId (Maybe XY)

newtype GardenShape' coords = GardenShape { _unGardenShape :: coords }
                            deriving (Eq, Show, ToJSON, FromJSON) via coords

type GardenShape = GardenShape' [Coord.XY]

makeLenses ''Garden'
makeAdaptorAndInstance' ''Garden'
makeLenses ''GardenId'
makeAdaptorAndInstance' ''GardenId'
makeLenses ''GardenShape'
makeAdaptorAndInstance' ''GardenShape'
makeLenses ''GardenPlant'
makeAdaptorAndInstance' ''GardenPlant'
