{-# LANGUAGE
    TypeFamilies
  , TemplateHaskell
  , TypeSynonymInstances
  , FlexibleInstances
  , TypeOperators
  , MultiParamTypeClasses
  , Arrows 
#-}
module Backend.Garden.Plant
  ( StoredPlant'(..)
  , StoredPlant
  , DBUpdate(AddPlant, DeletePlant)
  , DBSelect(SearchByName, GetAllPlants)
  )
where

import           Data.Aeson
import           Prelude                 hiding ( to )
import           Control.Lens            hiding ( ilike )
import           Control.Arrow                  ( returnA )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Opaleye
import "fht-api" Api.Garden.Plant
import "fht-data" Data.Garden.Plant
import "dbstorage-polysemy" Database.Storage
import "dbstorage-polysemy" Polysemy.Database

newtype StoredPlant' p = StoredPlant { _unStoredPlant :: p }
                       deriving (Eq, Show, ToJSON, FromJSON) via p

makeAdaptorAndInstance' ''StoredPlant'

-- | Hask side of a stored plant.
type StoredPlant = StoredPlant' Plant

type StoredPlantF = StoredPlant' PlantF

type PlantIdF = PlantId' (Field PGInt8)

type PlantF
  = Plant'
      PlantIdF
      (Field PGText)
      (FieldNullable PGText)
      (FieldNullable PGText)
      (FieldNullable PGTimestamptz)
      (Field (SqlArray PGMaintenanceType))
      (Field (SqlArray PGMaintenanceFreq))

instance DBIdentity StoredPlant where
  type DBId StoredPlant = PlantId
  dbId = _pId . _unStoredPlant

instance DBStorage StoredPlant where

  type DBError StoredPlant = Void
  
  data DBSelect StoredPlant = SearchByName Text
                            | GetAllPlants
  
  data DBUpdate StoredPlant = AddPlant Plant
                            | DeletePlant PlantId

  selectByIds ids = mkIdMap <$> trSelect (proc () -> byIdsQ -< ids)

  dbSelect = \case
    SearchByName name -> mkIdMap <$> trSelect (proc () -> byNameQ -< name)
    GetAllPlants      -> mkIdMap <$> trSelect allPlantsQ

  dbUpdate = \case
    AddPlant    p  -> undefined
    DeletePlant id -> undefined

plantTable :: Table StoredPlantF StoredPlantF
plantTable = table "plant" . pStoredPlant . StoredPlant . pPlant $ Plant
  { _pId               = pPlantId . PlantId . tableField $ "id"
  , _pName             = tableField "name"
  , _pDesc             = tableField "description"
  , _pImage            = tableField "image"
  , _pTimePlanted      = tableField "time_planted"
  , _pMaintenanceTypes = tableField "maintenance_types"
  , _pMaintenanceFreqs = tableField "maintenance_freqs"
  }

-- $dbQueries
-- Commonly used DB queries.

-- brittany-disable-next-binding
allPlantsQ :: Select StoredPlantF
allPlantsQ = selectTable plantTable

-- brittany-disable-next-binding
byIdsQ :: (Foldable f, Functor f) => SelectArr (f PlantId) StoredPlantF
byIdsQ = proc ids -> do
  p@(StoredPlant Plant {..}) <- allPlantsQ -< ()
  restrict -< _pId ^. unPlantId . to (in_ $ constant . _unPlantId <$> ids)
  returnA -< p

-- brittany-disable-next-binding
byNameQ :: SelectArr Text StoredPlantF
byNameQ = proc substring -> do
  p@(StoredPlant Plant {..}) <- selectTable plantTable -< ()
  let search = constant $ "%" <> substring <> "%"
  -- FIXME: check if the ordering of the fields is correct. The type of `ilike` offers no help here.
  restrict -< _pName `ilike` search
  returnA -< p
