{-# LANGUAGE
    Arrows
  , FlexibleInstances
  , TemplateHaskell
  , MultiParamTypeClasses
  , InstanceSigs
  , TypeApplications
#-}
module Backend.Garden.Plant.Logs
  ( DBUpdate(AddLogs)
  , DBSelect(..)
  , StoredLog'(..)
  , StoredLog
  )
where

import qualified Data.Map                      as M
import           Control.Arrow
import           Backend.Garden.Plant.Types     ( PlantIdF
                                                , PlantStorageErr(..)
                                                )
import           Polysemy.Error                as E
import "prelude-polysemy" Prelude.Control.Error
                                               as Err
import           Prelude                 hiding ( to )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Opaleye
import "fht-data" Data.Garden.Plant
import "dbstorage-polysemy" Database.Storage
import "dbstorage-polysemy" Polysemy.Database

data StoredLog' pid log = StoredLog
  { _slPlantId :: pid
  , _slLog     :: log
  }
  deriving (Eq, Show)

instance (Eq id, Ord log) => Ord (StoredLog' id log) where
  (StoredLog _ l0) `compare` (StoredLog _ l1) = l0 `compare` l1

type StoredLog = StoredLog' PlantId MaintenanceLog
type StoredLogF = StoredLog' PlantIdF MaintenanceLogF

type MaintenanceLogF
  = MaintenanceLog' (Field PGMaintenanceType) (Field PGTimestamptz)

makeAdaptorAndInstance' ''StoredLog'

-- | The only reasonable way to get an id from a `StoredLog` is to treat the whole row as a
-- unique ID.
instance DBIdentity StoredLog where
  type DBId StoredLog = StoredLog
  dbId = identity

instance DBStorage StoredLog where

  type UpdateConstraints StoredLog = '[E.Error Err.KnownError]
  type DBError StoredLog = PlantStorageErr
  
  data DBUpdate StoredLog =
    -- | Store the logs without any checks. 
    UnsafeAddLogs [StoredLog]
    -- | Store the log with checks.
    | AddLogs PlantId [MaintenanceLog]
  
  data DBSelect StoredLog =
    -- | Get all logs for the plant.
    GetForPlant PlantId
    | GetForPlants [PlantId]

  selectByIds logs =
    let ids = toList $ _slPlantId <$> logs
    -- currently inefficient, select all logs by plant (possibly returning redundant rows)
    -- and restrict upon the returned rows. 
    in  M.filter (`elem` logs) <$> dbSelect (GetForPlants ids)

  dbUpdate
    :: UpdateOf StoredLog r => DBUpdate StoredLog -> Sem r [DBId StoredLog]
  dbUpdate = \case
    AddLogs id mls ->
      let logs = StoredLog id <$> mls
          -- try to find the log, if it doesn't yet exist, insert one.
      in  selectByIds @StoredLog logs
            >>= maybe (dbUpdate . UnsafeAddLogs $ logs) alreadyExists
            .   find (`elem` logs)
            .   M.elems
      where alreadyExists = throwKnownError . LogAlreadyExists . _slLog

    UnsafeAddLogs sls ->
      trInsertManyReturning logsTable (toFields <$> sls) identity

  dbSelect = \case
    GetForPlants ids -> trSelect (proc () -> byPlantIdsQ -< ids) <&> mkMap
    GetForPlant  id  -> dbSelect . GetForPlants $ [id]

mkMap :: Ord a => [a] -> Map a a
mkMap logs = M.fromList [ (log', log') | log' <- logs ]

logsTable :: Table StoredLogF StoredLogF
logsTable = table "plant_maintenance_log" . pStoredLog $ StoredLog
  { _slPlantId = pPlantId . PlantId . tableField $ "plant_id"
  , _slLog     = pMaintenanceLog $ MaintenanceLog
                   { _mlMaintenance   = tableField "maintenance"
                   , _mlTimePerformed = tableField "time_performed"
                   }
  }

-- brittany-disable-next-binding
byPlantIdsQ :: (Foldable f, Functor f) => SelectArr (f PlantId) StoredLogF
byPlantIdsQ = proc ( fmap (_unPlantId . constant) -> ids) -> do
  sl@StoredLog {..} <- selectTable logsTable -< ()
  restrict -< in_ ids (_unPlantId _slPlantId)
  returnA -< sl

