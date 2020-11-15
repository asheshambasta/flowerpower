module Main
  ( main
  )
where


import           Servant.Server                 ( Handler(..) )

import           Polysemy
import           Polysemy.Reader
import "dbstorage-polysemy" Polysemy.Database
                                         hiding ( Runtime )

import           Network.Wai.Handler.Warp      as Warp
import qualified Backend.Api.Garden.Plant      as PlantApi
import           Backend.Runtime

main :: IO ()
main = undefined

plantServer :: Runtime -> IO ()
plantServer rt =
  let app = PlantApi.plantApiApplication (sem2Handler rt)
  in  Warp.runSettings undefined app

sem2Handler
  :: Runtime
  -> (Sem '[Transaction , Error KnownError , Reader Logger] a -> Handler a)
sem2Handler rt sem = Handler $ undefined
