{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE
    TemplateHaskell
  , FlexibleInstances  
  , MultiParamTypeClasses  
#-}

{-|
Module: Backend.Garden.Plant.Orphans
Description: Orphans needed for enums and all. 
-}
module Backend.Garden.Plant.Orphans
  (
  -- ** Enums
    PGMaintenanceType
  , PGMaintenanceFreq
  )
where

import "fht-data" Data.Garden.Plant             ( MaintenanceFreq
                                                , MaintenanceType
                                                )
import "composite-opaleye" Composite.Opaleye.TH ( deriveOpaleyeEnum )

deriveOpaleyeEnum ''MaintenanceFreq "maintenance_freq" (Just . identity)
deriveOpaleyeEnum ''MaintenanceType "maintenance_type" (Just . identity)
