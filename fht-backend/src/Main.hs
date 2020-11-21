{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  )
where


import qualified Control.Monad.Log             as MLog
import "prelude-polysemy" Prelude.Polysemy.ID
import "prelude-polysemy" Prelude.Control.Error ( logErrors
                                                , asServantError
                                                )
import           Servant.Server                 ( Handler(..) )

import           Polysemy
import           Polysemy.Reader
import "dbstorage-polysemy" Polysemy.Database
                                         hiding ( Runtime )

import           Network.Wai.Handler.Warp      as Warp
import qualified Backend.Api.Garden.Plant      as PlantApi
import qualified Options.Applicative           as A
import           Backend.Runtime

main :: IO ExitCode
main = do
  conf <- A.execParser parseConf
  eRt  <- conf2Runtime conf
  case eRt of
    Left  rtErr -> putStrLn (show @RuntimeError @Text rtErr) $> ExitFailure (-1)
    Right rt@Runtime { _rLogger = Logger _ l } -> MLog.runLogT' l $ do
      MLog.info "Starting servers."
      liftIO $ plantServer rt

parseConf :: A.ParserInfo Conf
parseConf = A.info
  (confP <**> A.helper)
  (A.fullDesc <> A.progDesc "Run the Flowerpower backend service." <> A.header
    "Flowerpower!"
  )

plantServer :: Runtime -> IO ExitCode
plantServer rt =
  let app = PlantApi.plantApiApplication (sem2Handler rt)
  in  try (Warp.runSettings Warp.defaultSettings app) >>= \case
        Left err ->
          putStrLn (show @SomeException @Text err) $> ExitFailure (-1)
        Right _ -> pure ExitSuccess

sem2Handler
  :: Runtime
  -> (  Sem
         '[ Transaction
          , Error KnownError
          , ID
          , Reader IDGen
          , Logging
          , Reader Logger
          , Reader DBRuntime
          , Embed IO
          ]
         a
     -> Handler a
     )
sem2Handler Runtime {..} sem =
  let runSem = sem & flip runTransactionEmbed runToIO & runToIO
  -- Wer're going from: IO (Either KnownError a) -> ExceptT ServerError IO a 
  in  Handler $ liftIO runSem >>= either (throwError . asServantError) pure
 where
  runToIO sem' =
    sem'
      & logErrors
      & runError
      & runIDGenIO
      & Polysemy.Reader.runReader _rIDGen
      & runLoggingIO
      & Polysemy.Reader.runReader _rLogger
      & Polysemy.Reader.runReader _rDBRuntime
      & runM
