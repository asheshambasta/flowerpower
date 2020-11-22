module Backend.Garden.Plant.Types
  ( PlantIdF
  , PlantStorageErr(..)
  )
where

import "fht-data" Data.Garden.Plant
import           Opaleye

type PlantIdF = PlantId' (Field PGInt8)

-- * Errors 

data PlantStorageErr = AlreadyExists PlantId
                     | StorageError Text
                     | NoIdSupplied
                     deriving Show

instance IsKnownError PlantStorageErr where
  errCode AlreadyExists{} = errCode' "ALREADY_EXISTS"
  errCode StorageError{}  = errCode' "STORAGE_ERROR"
  errCode NoIdSupplied{}  = errCode' "NO_ID_SUPPLIED"

  userMessage (AlreadyExists id) =
    Just . mappend "Already exists: " . show $ id
  userMessage NoIdSupplied{} = Just "Invalid API call: no id present!"
  userMessage _              = Nothing

  httpStatus AlreadyExists{} = conflict409
  httpStatus NoIdSupplied    = badRequest400
  httpStatus StorageError{}  = internalServerError500

  errorLogLevel _ = levelCritical

errCode' :: ErrCode -> ErrCode
errCode' = mappend "ERR.PLANT."
