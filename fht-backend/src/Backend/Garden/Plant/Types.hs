module Backend.Garden.Plant.Types
  ( PlantIdF
  , PlantStorageErr(..)
  )
where

import "fht-data" Data.Garden.Plant
import           Opaleye

type PlantIdF = PlantId' (Field PGInt8)

-- * Errors 

data PlantStorageErr = PlantAlreadyExists PlantId
                     | LogAlreadyExists MaintenanceLog
                     | StorageError Text
                     | NoIdSupplied
                     deriving Show

instance IsKnownError PlantStorageErr where
  errCode PlantAlreadyExists{} = errCode' "PLANT_ALREADY_EXISTS"
  errCode LogAlreadyExists{}   = errCode' "LOG_ALREADY_EXISTS"
  errCode StorageError{}       = errCode' "STORAGE_ERROR"
  errCode NoIdSupplied{}       = errCode' "NO_ID_SUPPLIED"

  userMessage (PlantAlreadyExists id) =
    Just . mappend "Plant already exists: " . show $ id
  userMessage (LogAlreadyExists log') =
    Just . mappend "Log exists: " . show $ log'
  userMessage NoIdSupplied{} = Just "Invalid API call: no id present!"
  userMessage _              = Nothing

  httpStatus PlantAlreadyExists{} = conflict409
  httpStatus LogAlreadyExists{}   = conflict409
  httpStatus NoIdSupplied         = badRequest400
  httpStatus StorageError{}       = internalServerError500

  errorLogLevel _ = levelCritical

errCode' :: ErrCode -> ErrCode
errCode' = mappend "ERR.PLANT."
