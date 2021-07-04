{-# LANGUAGE
    DeriveAnyClass
#-}
module Data.Property.Garden
  () where

import           Data.Aeson
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

-- | A garden: a garden can belong to exactly one property.
--
-- A garden can have a shape: described by cartesian coordinates. 
data Garden' id gtype name desc = Garden
  { _gId   :: id
  , _gType :: gtype
  , _gName :: name
  , _gDesc :: desc
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GardenType = Interior | Exterior
                deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GardenId' id = GardenId { _unGardenId :: id }
                   deriving (Eq, Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData , Ord, Num, Real, Enum, Integral) via id

type GardenId = GardenId' Int64

