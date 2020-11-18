{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Backend.Runtime
  ( module DB
  , module L
  , Conf(..)
  , Runtime(..)
  , RuntimeError(..)
  , confP
  -- * Generating runtimes. 
  , conf2Runtime
  )
where

import           Control.Monad.Log             as ML
import "prelude-polysemy" Prelude.Control.Log  as L
import "dbstorage-polysemy" Database.Runtime   as DB
import qualified Options.Applicative           as A

data Conf = Conf
  { _cDBConf          :: DB.DBConf
  , _cLogLevel        :: ML.Level
  , _cApplicationName :: Text
  }

-- | The runtime is nothing but a product of materialised values useful for running the application.
data Runtime = Runtime
  { _rDBRuntime :: DBRuntime
  , _rLogger    :: L.Logger
  , _rConf      :: Conf
  }

confP :: A.Parser Conf
confP = do
  _cDBConf          <- DB.dbConfP
  _cLogLevel        <- parseLogLevel
  _cApplicationName <- parseAppName
  pure Conf { .. }
 where
  parseAppName = A.strOption
    (  A.long "application-name"
    <> A.short 'N'
    <> A.help "Application name"
    <> A.value "Flowerpower"
    <> A.showDefault
    )
  parseLogLevel = A.option
    (A.eitherReader readEither)
    (  A.long "log-level"
    <> A.short 'L'
    <> A.help "Logging level for the application"
    <> A.value L.levelDebug
    <> A.showDefault
    )

conf2Runtime :: Conf -> IO (Either RuntimeError Runtime)
conf2Runtime _rConf@Conf {..} = do

  mlLogger <- createLogger
  eDb      <- try $ createRuntime _cDBConf
  let _rLogger = L.Logger logEnv mlLogger

  pure $ case eDb of
    Left  err         -> Left . RuntimeDBError $ err
    Right _rDBRuntime -> Right Runtime { .. }

 where
  logEnv       = L.Env _cApplicationName
  createLogger = ML.makeDefaultLogger ML.simpleTimeFormat
                                      (ML.LogStdout 4096)
                                      _cLogLevel
                                      logEnv

data RuntimeError where
  RuntimeDBError ::DBError -> RuntimeError

deriving instance Show RuntimeError
deriving instance Exception RuntimeError
