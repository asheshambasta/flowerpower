{-# LANGUAGE
    TypeFamilies
  , TemplateHaskell
  , FlexibleInstances
  , TypeOperators
  , MultiParamTypeClasses
  , Arrows 
  , InstanceSigs
  , TypeApplications
#-}
module Backend.Garden.Plant
  ( StoredPlant'(..)
  , StoredPlant
  , DBUpdate(AddPlant, UpdatePlant, DeletePlant)
  , DBSelect(SearchByName, GetAllPlants)
  -- * Getting data
  , populatePlantData
  )
where

import           Backend.Garden.Plant.Orphans   ( PGMaintenanceType
                                                , PGMaintenanceFreq
                                                )
import qualified Data.Map                      as M
import           Backend.Garden.Plant.Types     ( PlantIdF )
import qualified Backend.Garden.Plant.Logs     as Logs
import           Polysemy.Error                as E
import qualified "prelude-polysemy" Prelude.Polysemy.ID
                                               as ID
import "prelude-polysemy" Prelude.Control.Error
                                               as Err
import           Data.Aeson
import           Prelude                 hiding ( to )
import           Control.Arrow                  ( returnA )
import           Control.Lens            hiding ( ilike )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Opaleye
import "fht-data" Data.Garden.Plant
import "dbstorage-polysemy" Database.Storage
import "dbstorage-polysemy" Polysemy.Database

newtype StoredPlant' p = StoredPlant { _unStoredPlant :: p }
                       deriving (Eq, Show, ToJSON, FromJSON) via p

makeAdaptorAndInstance' ''StoredPlant'
makeLenses ''StoredPlant'

-- | Hask side of a stored plant.
type StoredPlant = StoredPlant' Plant

type StoredPlantF = StoredPlant' PlantF

type PlantF
  = Plant'
      PlantIdF
      (Field PGText)
      (FieldNullable PGText)
      (FieldNullable PGText)
      (Field PGDate)
      (Field (SqlArray PGMaintenanceType))
      (Field (SqlArray PGMaintenanceFreq))

instance DBIdentity StoredPlant where
  type DBId StoredPlant = PlantId
  dbId = _pId . _unStoredPlant

instance DBStorage StoredPlant where

  type UpdateConstraints StoredPlant = '[E.Error Err.KnownError , ID.ID]
  type SelectConstraints StoredPlant = '[E.Error Err.KnownError , Embed IO]

  type DBError StoredPlant = Void
  
  data DBSelect StoredPlant = SearchByName Text
                            | GetAllPlants
  
  data DBUpdate StoredPlant = AddPlant Plant
                            | UpdatePlant Plant
                            | DeletePlant PlantId

  selectByIds ids = mkIdMap <$> trSelect (proc () -> byIdsQ -< ids)

  dbSelect = \case
    SearchByName name -> mkIdMap <$> trSelect (proc () -> byNameQ -< name)
    GetAllPlants      -> mkIdMap <$> trSelect allPlantsQ

  dbUpdate
    :: UpdateOf StoredPlant r
    => DBUpdate StoredPlant
    -> Sem r [DBId StoredPlant]
  dbUpdate = \case
    AddPlant p@Plant {..} -> do
      newId <- ID.newSnowflake
      let pid = PlantId . fromIntegral . ID.snowflakeToInteger $ newId
      trInsertManyReturning plantTable
                            [toFields . StoredPlant @Plant $ p & pId .~ pid]
                            (view $ unStoredPlant . pId)

    UpdatePlant (toFields @StoredPlant @StoredPlantF . StoredPlant -> p@(StoredPlant Plant{..}))
      -> trUpdateReturning plantTable
                           (const p) -- set the whole row
                           (view $ unStoredPlant . pId . to (.=== _pId))
                           (view $ unStoredPlant . pId)

    DeletePlant id -> trDeleteReporting
      id
      plantTable
      (view $ unStoredPlant . pId . to (.=== constant id))

-- | Get full plant data
-- TODO: implement
populatePlantData
  :: ( Foldable f
     , Functor f
     , Traversable f
     , SelectOf StoredPlant r
     , SelectOf Logs.StoredLog r
     )
  => f Plant
  -> Sem r (f FullPlantData)
populatePlantData ps = do
  logs <- dbSelect . Logs.GetForPlants $ ids
  mapM (addLogs logs) ps
 where
  ids = toList $ _pId <$> ps
  addLogs allLogs p@Plant {..} =
    FullPlantData p <$> maintenanceStatusesNow maints plantLogs _pDayPlanted
   where
    maints = p ^. pMaintenancesList
    plantLogs =
      fmap Logs._slLog
        . M.elems
        . M.filter ((== _pId) . Logs._slPlantId)
        $ allLogs

plantTable :: Table StoredPlantF StoredPlantF
plantTable = table "plant" . pStoredPlant . StoredPlant . pPlant $ Plant
  { _pId               = pPlantId . PlantId . tableField $ "plant_id"
  , _pName             = tableField "plant_name"
  , _pDesc             = tableField "description"
  , _pImage            = tableField "image"
  , _pDayPlanted       = tableField "day_planted"
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
