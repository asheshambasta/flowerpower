module Backend.Garden.Plant.Types
  ( PlantIdF
  , PlantStorageErr(..)
  )
where

import "prelude-polysemy" Prelude.Control.Error

import "fht-data" Data.Garden.Plant
import           Opaleye

type PlantIdF = PlantId' (Field PGInt8)

-- * Errors 

data PlantStorageErr = AlreadyExists PlantId
                     | StorageError Text
                     deriving Show

instance IsKnownError PlantStorageErr where

