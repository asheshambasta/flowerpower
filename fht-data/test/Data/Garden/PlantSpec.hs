{-# LANGUAGE TypeApplications #-}
module Data.Garden.PlantSpec
  ( spec
  )
where

import qualified Data.Map                      as M
import qualified Data.Time                     as Time
import           Protolude
import           Test.Hspec
import qualified Data.Garden.Plant             as P
import qualified Test.QuickCheck               as QC
import qualified Test.QuickCheck.Monadic       as QC

spec :: Spec
spec =
  describe "Data.Garden.Plant" . sequence_ $ [maintenanceStatus, maintStatuses]

maintenanceStatus =
  describe "MaintenanceStatus"
    $ it "Must report the right status depending on the time differences."
    . QC.property
    $ checkTimes

checkTimes :: QC.Property
checkTimes = QC.monadicIO $ do
  (diffTime, _, mstat) <- randMaintenanceStats
  pure $ case mstat of
    P.DueIn{} -> diffTime <= 0
    P.DueBy{} -> diffTime >= 0

randMaintenanceStats = do
  time0    <- genCurTime
  diffTime <- genDiffTime
  freq     <- QC.pick $ QC.arbitrary @P.MaintenanceFreq
  -- let time1 = Time.addUTCTime diffTime time0
  pure (diffTime, freq, P.maintenanceStatus freq diffTime)

genCurTime = QC.run Time.getCurrentTime
genDiffTime = QC.pick $ fromIntegral <$> QC.arbitrary @Integer

maintStatuses =
  describe "MaintenaceTypes"
    $ it
        "Must contain the right MaintenanceTypes in the map of statuses & vice-versa."
    . QC.property
    . QC.monadicIO
    $ do
        (logs, time, maints) <- genValues
        let statuses = P.maintenanceStatuses maints logs time
            keys     = M.keys statuses
            mTypes   = P._mType <$> maints
        pure $ all (`elem` keys) mTypes && all (`elem` mTypes) keys
 where
  genValues = do
    logs   <- genLogs
    time   <- genCurTime
    maints <- QC.pick . QC.listOf $ QC.arbitrary @P.Maintenance
    pure (logs, time, maints)


genLogs = do
  num <- abs <$> QC.pick (QC.arbitrary @Int)
  replicateM num genLog
 where
  mtype  = QC.pick $ QC.arbitrary @P.MaintenanceType
  genLog = P.MaintenanceLog <$> mtype <*> genCurTime

instance QC.Arbitrary P.MaintenanceType where
  arbitrary = QC.elements $ enumFromTo minBound maxBound

instance QC.Arbitrary P.MaintenanceFreq where
  arbitrary = QC.elements $ enumFromTo minBound maxBound

instance QC.Arbitrary P.Maintenance where
  arbitrary = P.Maintenance <$> QC.arbitrary <*> QC.arbitrary
