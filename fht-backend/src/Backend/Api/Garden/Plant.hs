{-# LANGUAGE TypeApplications #-}
module Backend.Api.Garden.Plant
  ( module Api.Garden.Plant
  )
where

import           Servant.API
import           Servant.Server
import           Backend.Garden.Plant
import           Backend.Runtime
import "fht-api" Api.Garden.Plant

plantApiServer :: Server PlantApi
plantApiServer = undefined :<|> undefined :<|> undefined :<|> undefined

plantApiApplication :: Application
plantApiApplication = serve (Proxy @PlantApi) plantApiServer
