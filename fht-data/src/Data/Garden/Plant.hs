{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
module Data.Garden.Plant
  ( Plant'(..)
  , Plant
  , PlantId'(..)
  , PlantId
  , FullPlantData(..)
  -- ** MaintenanceType of plants
  , MaintenanceType(..)
  , MaintenanceFreq(..)
  , Maintenances
  , Maintenance(..)
  , freqDiffTime
  -- *** Status of maintenance.
  -- Data constructors of MaintenanceStatus are not exported, use the Patterns instead.
  , MaintenanceStatus
  , maintenanceStatus
  , MaintenanceStatuses
  , containsDues
  , isDue
  , filterDues
  -- *** Patterns for safe construction/matches.
  , pattern DueIn
  , pattern DueBy
  , _UnsafeDueIn
  , _UnsafeDueBy
  -- , _DueBy
  -- *** Logging maintenance
  , MaintenanceLog'(..)
  , MaintenanceLog
  -- *** Finding the latest 
  , latestMaintenances
  , maintenanceStatuses
  , maintenanceStatusesNow
  -- * Lenses
  -- ** Plant
  , unPlantId
  , pId
  , pName
  , pDesc
  , pImage
  , pDayPlanted
  , pMaintenanceTypes
  , pMaintenanceFreqs
  , pMaintenances
  , fpdPlant
  , fpdMStatuses
  -- ** Maintenance
  , mFreq
  , mType
  -- ** MaintenanceLog
  , mlMaintenance
  , mlTimePerformed
  -- * DB
  -- ** Enums
  , PGMaintenanceType
  , PGMaintenanceFreq
  -- ** Profunctor
  , pPlantId
  , pPlant
  , pMaintenanceLog
  )
where

import           Composite.Opaleye.TH           ( deriveOpaleyeEnum )
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Web.HttpApiData                ( ToHttpApiData
                                                , FromHttpApiData
                                                )
import           Control.Lens
import qualified Data.Map                      as M
import           Data.Aeson
import qualified Data.Time                     as Time

data MaintenanceType = Pruning | Fertilizing | Repotting
                 deriving (Eq, Show, Enum, Ord, Bounded, Generic, ToJSON, ToJSONKey, FromJSON, FromJSONKey)

deriveOpaleyeEnum ''MaintenanceType "maintenance_type" (Just . identity)

data MaintenanceFreq = Week | Month | Year
                     deriving (Eq, Show, Enum, Ord, Bounded, Generic, ToJSON, FromJSON)

-- | Reports the frequencies that apply for a given value of elapsed days.
freqsForDays
  :: Time.Day -> Time.Day -> M.Map MaintenanceFreq Time.NominalDiffTime
freqsForDays later earlier
  | earlier < later = M.fromList $ forFreq Week <> forFreq Month <> forFreq Year
  | otherwise       = mempty
 where
  days = Time.diffDays earlier later
  forFreq f =
    [ (f, freqDiffTime f * cycles)
    | days `mod` freqDays f == 0
    , let cycles = fromIntegral $ days `quot` fd
          fd     = freqDays f
    ]

-- | Get the difftime by frequency.
freqDiffTime :: MaintenanceFreq -> Time.NominalDiffTime
freqDiffTime = \case
  Week  -> freqDays Week * 24 * 3600
  Month -> freqDays Month * freqDiffTime Week
  Year  -> freqDays Year * freqDiffTime Month

freqDays :: Num i => MaintenanceFreq -> i
freqDays = \case
  Week  -> 7
  Month -> 30
  Year  -> 365

deriveOpaleyeEnum ''MaintenanceFreq "maintenance_freq" (Just . identity)

data MaintenanceStatus = UnsafeDueIn MaintenanceFreq Time.NominalDiffTime
                       | UnsafeDueBy MaintenanceFreq (Maybe Time.NominalDiffTime)
                       deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

makePrisms ''MaintenanceStatus

{-# COMPLETE DueIn, DueBy #-}
pattern DueIn :: MaintenanceFreq -> Time.NominalDiffTime -> MaintenanceStatus
pattern DueIn f t <- UnsafeDueIn f t where
  DueIn f t = maintenanceStatus f t

pattern DueBy
  :: MaintenanceFreq -> Maybe Time.NominalDiffTime -> MaintenanceStatus
pattern DueBy f t <- UnsafeDueBy f t where
  DueBy f mt = maybe (UnsafeDueBy f Nothing) (maintenanceStatus f) mt

-- | Smart constructor for maintenance statuses.
maintenanceStatus
  :: MaintenanceFreq -> Time.NominalDiffTime -> MaintenanceStatus
maintenanceStatus f t | t < 0     = UnsafeDueIn f t
                      | otherwise = UnsafeDueBy f (Just t)

newtype PlantId' id = PlantId { _unPlantId :: id }
                 deriving ( Eq, Show, ToJSON
                          , FromJSON, ToHttpApiData, FromHttpApiData
                          , Ord, Num, Real, Enum, Integral
                          ) via id

type PlantId = PlantId' Int64

makeLenses ''PlantId'
makeAdaptorAndInstance' ''PlantId'

data Plant' id name desc img time maint maintFreq = Plant
  { _pId               :: id
  , _pName             :: name
  , _pDesc             :: desc -- ^ Description 
  , _pImage            :: img -- ^ An image (if available)
  , _pDayPlanted       :: time -- ^ Time at which this was planted
  , _pMaintenanceTypes :: maint
  , _pMaintenanceFreqs :: maintFreq
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeLenses ''Plant'
makeAdaptorAndInstance' ''Plant'

pMaintenances :: Lens' Plant Maintenances
pMaintenances = lens from' to'
 where
  from' Plant {..} = M.fromList $ zip _pMaintenanceTypes _pMaintenanceFreqs
  to' p (M.toList -> ms) =
    let ms' = fst <$> ms
        fs' = snd <$> ms
    in  p & pMaintenanceTypes .~ ms' & pMaintenanceFreqs .~ fs'

type Plant
  = Plant'
      PlantId
      Text
      (Maybe Text)
      (Maybe Text)
      Time.Day
      [MaintenanceType]
      [MaintenanceFreq]

data Maintenance = Maintenance
  { _mType :: MaintenanceType
  , _mFreq :: MaintenanceFreq
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type Maintenances = M.Map MaintenanceType MaintenanceFreq

makeLenses ''Maintenance

-- | Log of the maintenance performed on the plant.
data MaintenanceLog' maint performedDate = MaintenanceLog
  { _mlMaintenance   :: maint
  , _mlTimePerformed :: performedDate
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type MaintenanceLog = MaintenanceLog' MaintenanceType Time.UTCTime

type MaintenanceStatuses = M.Map MaintenanceType MaintenanceStatus

makeAdaptorAndInstance' ''MaintenanceLog'

-- | Check if a map contains at least 1 due status.
containsDues :: MaintenanceStatuses -> Bool
containsDues = (/= M.empty) . filterDues

-- | Get all the due statuses. 
filterDues :: MaintenanceStatuses -> MaintenanceStatuses
filterDues = M.filter isDue

isDue :: MaintenanceStatus -> Bool
isDue = isJust . preview _UnsafeDueBy

makeLenses ''MaintenanceLog'

-- | Get the report of the latest maintenances.
latestMaintenances
  :: Foldable f => f MaintenanceLog -> M.Map MaintenanceType Time.UTCTime
latestMaintenances = foldl' addIfLater mempty
 where
  addIfLater acc MaintenanceLog {..} = M.alter ifEarlierOrNever
                                               _mlMaintenance
                                               acc
   where
    ifEarlierOrNever = Just . maybe _mlTimePerformed (max _mlTimePerformed)

{- |
Get the diff times of all maintenances.
For all the known latest maintenances, we'd like to see if each of the given maintenances to perform are present in the latest times.
If they aren't, it just means the maintenance was never performed. If they are, we determine based on the time of performing the maintenance.
-}
maintenanceStatuses
  :: Foldable f
  => [Maintenance] -- ^ List of maintenances to perform.
  -> f MaintenanceLog -- ^ Log of all performed maintenances. 
  -> Time.Day -- ^ Day the plant was planted
  -> Time.UTCTime -- ^ The time at which to determine due maintenances.
  -> MaintenanceStatuses -- ^ Map containing all maintenance types and the difference of time since the last time they were performed.
maintenanceStatuses maints (latestMaintenances -> latest) dayPlanted atTime@Time.UTCTime { utctDay = atDay }
  = let logBased         = foldl' determineStatus mempty maints
        datePlantedBased = undefined
    in  datePlantedBased <> logBased
 where
  determineStatus statuses Maintenance {..} = case M.lookup _mType latest of
    -- never performed. 
    Nothing -> addToStats $ DueBy _mFreq Nothing
    Just lastPerformed ->
      let timeSincePerformed = Time.diffUTCTime atTime lastPerformed
      in  addToStats $ maintenanceStatus
            _mFreq
            (freqDiffTime _mFreq - timeSincePerformed)
    where addToStats v = M.insert _mType v statuses
  reqFreqsForDays  = freqsForDays atDay dayPlanted
  daysSincePlanted = Time.diffDays dayPlanted atDay
  reqMaintsForDays =
    [ (_mType, undefined) | Maintenance {..} <- maints, undefined ] -- _mFreq `elem` reqFreqsForDays

-- | Get the maintenance statuses at the current time.
-- See `maintenanceStatuses`
maintenanceStatusesNow
  :: (MonadIO m, Foldable f)
  => [Maintenance] -- ^ Required maintenance & frequencies
  -> f MaintenanceLog -- ^ Perfomed maintenance logs 
  -> Time.Day -- ^ Day on which the plant was planted. 
  -> m MaintenanceStatuses
maintenanceStatusesNow maints log' dayPlanted =
  maintenanceStatuses maints log' dayPlanted <$> liftIO Time.getCurrentTime

-- | Full plant data: contains information about the plant as well its maintenance statuses.
data FullPlantData = FullPlantData
  { _fpdPlant     :: Plant
  , _fpdMStatuses :: MaintenanceStatuses
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

makeLenses ''FullPlantData
