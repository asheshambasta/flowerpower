{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Backend.Runtime
  ( module DB
  , module L
  , Conf(..)
  , Runtime(..)
  , RuntimeError(..)
  , StaticFilesDir(..)
  , confP
  -- * Lenses
  , cDBConf
  , cLogLevel
  , cApplicationName
  , cSnowflakeNode
  , cCorsAllowedOrigins
  , cStaticFilesDir
  , rDBRuntime
  , rLogger
  , rConf
  , rIDGen
  -- * Generating runtimes. 
  , conf2Runtime
  )
where

import "filepath" System.FilePath               ( addTrailingPathSeparator )
import "wai-cors" Network.Wai.Middleware.Cors   ( Origin )

import           Control.Lens
import           Control.Monad.Log             as ML

import "prelude-polysemy" Prelude.Polysemy.ID
import "prelude-polysemy" Prelude.Control.Log  as L

import "dbstorage-polysemy" Database.Runtime   as DB

import qualified Options.Applicative           as A

newtype StaticFilesDir = StaticFilesDir FilePath deriving (Eq, Show) via FilePath

data Conf = Conf
  { _cDBConf             :: DB.DBConf
  , _cLogLevel           :: ML.Level
  , _cApplicationName    :: Text
  , _cSnowflakeNode      :: Integer
  , _cCorsAllowedOrigins :: [Origin]
  , _cStaticFilesDir     :: StaticFilesDir
  }

-- | The runtime is nothing but a product of materialised values useful for running the application.
data Runtime = Runtime
  { _rDBRuntime :: DBRuntime
  , _rLogger    :: L.Logger
  , _rConf      :: Conf
  , _rIDGen     :: IDGen
  }

makeLenses ''Runtime
makeLenses ''Conf

confP :: A.Parser Conf
confP = do
  _cDBConf             <- DB.dbConfP
  _cLogLevel           <- parseLogLevel
  _cApplicationName    <- parseAppName
  _cSnowflakeNode      <- parseSnowflakeNode
  _cCorsAllowedOrigins <- parseAllowedOrigins
  _cStaticFilesDir     <- parseStaticFilesDir
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
  parseSnowflakeNode = A.option
    A.auto
    (  A.long "snowflake-node"
    <> A.short 'S'
    <> A.help "Snowflake node for generating ids"
    <> A.value 0
    <> A.showDefault
    )
  parseAllowedOrigins = A.many $ A.strOption
    (A.long "cors-allowed-origin" <> A.short 'C' <> A.help
      "Allowed CORS origins"
    )
  parseStaticFilesDir =
    StaticFilesDir . addTrailingPathSeparator <$> A.strOption
      (  A.long "static-files-dir"
      <> A.help "Directory to serve static files from."
      <> A.showDefault
      <> A.value "/var/www"
      )

conf2Runtime :: Conf -> IO (Either RuntimeError Runtime)
conf2Runtime _rConf@Conf {..} = do

  mlLogger <- createLogger
  eDb      <- try $ createRuntime _cDBConf
  _rIDGen  <- newIDGen defaultConfig _cSnowflakeNode
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
