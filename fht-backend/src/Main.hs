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
main = do
  conf <- undefined
  eRt  <- conf2Runtime conf
  case eRt of
    Left  rtErr -> undefined
    Right rt    -> undefined

plantServer :: Runtime -> IO ()
plantServer rt =
  let app = PlantApi.plantApiApplication (sem2Handler rt)
  in  Warp.runSettings undefined app

sem2Handler
  :: Runtime
  -> (  Sem
         '[ Transaction
          , Error KnownError
          , Reader Logger
          , Reader DBRuntime
          , Embed IO
          ]
         a
     -> Handler a
     )
sem2Handler rt@Runtime {..} sem =
  let runSem = do
        eRes <- sem & flip runTransactionEmbed runToIO & runToIO
        pure eRes
  in  Handler $ undefined
 where
  runToIO sem' =
    sem'
      & runError
      & Polysemy.Reader.runReader _rLogger
      & Polysemy.Reader.runReader _rDBRuntime
      & runM
