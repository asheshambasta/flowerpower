{-# LANGUAGE TemplateHaskell, BlockArguments, StrictData, TypeApplications
           , TypeOperators, DataKinds, PolyKinds, LambdaCase #-}
module Control.Log
  ( Logging(..)
  -- * Environment
  , Env(..)
  , envText
  , localEnv
  , underEnv
  -- * Logging functions.
  , debug
  , info
  , warn
  , error
  , critical
  -- * Run loggers
  , runLoggingIO
  -- * Re-exports
  , L.Level(..)
  )
where

import           Control.Lens
import qualified Data.Text                     as T
import qualified Data.String                    ( IsString(..) )
import           Protolude               hiding ( Reader )
import qualified Control.Monad.Log             as L
import           Polysemy
import           Polysemy.Reader               as R

-- | Logging env.
data Env = Env Text
         | Nested Text Env
         deriving (Eq, Show)

envText :: Env -> Text
envText (Nested txt env') = txt <> "/" <> envText env'
envText (Env txt        ) = txt

instance Semigroup Env where
  (Env txt     ) <> e1         = Nested txt e1
  (Nested txt e) <> e'@Env{}   = (Nested txt $ e <> e')
  (Nested txt e) <> n@Nested{} = Nested txt $ e <> n

-- | Logger with environment.
data Logger = Logger Env (L.Logger Env)

makePrisms ''Logger

instance IsString Env where
  fromString = Env . T.pack

data Logging m a where
  Debug ::Text -> Logging m ()
  Info ::Text -> Logging m ()
  Warn ::Text -> Logging m ()
  Error ::Text -> Logging m ()
  Critical ::Text -> Logging m ()

makeSem ''Logging

localEnv
  :: forall a r
   . Member (Reader Logger) r
  => (Env -> Env)
  -> Sem r a
  -> Sem r a
localEnv modify' = R.local (over (_Logger . _1) modify')

underEnv :: forall a r . Member (Reader Logger) r => Env -> Sem r a -> Sem r a
underEnv new = localEnv (<> new)

runLoggingIO
  :: forall a r
   . Members '[Embed IO , Reader Logger] r
  => Sem (Logging ': r) a
  -> Sem r a
runLoggingIO = interpret \case
  Debug    msg -> msg `withLogger` L.debug'
  Info     msg -> msg `withLogger` L.info'
  Warn     msg -> msg `withLogger` L.warning'
  Critical msg -> msg `withLogger` L.critical'

withLogger
  :: forall a r
   . Members '[Embed IO , Reader Logger] r
  => Text
  -> (Env -> Text -> L.LogT Env IO a)
  -> Sem r a
withLogger msg logFunc = do
  Logger env logger <- R.ask
  embed . L.runLogT' logger . logFunc env $ msg

