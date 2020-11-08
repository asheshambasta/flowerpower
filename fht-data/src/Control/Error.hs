{-# LANGUAGE DefaultSignatures #-}
module Control.Error
  ( ErrCode(..)
  , FlError(..)
  , IsFlError(..)

  -- * Throwing errors.
  , throwFlError

  -- * Handy re-exports.
  , module HTTP
  , module Log
  , module E
  )
where

import qualified Data.Text                     as T
import qualified GHC.Show
import           Polysemy.Error                as E
import           Polysemy
import           Control.Log                   as Log
import           Network.HTTP.Types            as HTTP
import           Protolude

newtype ErrCode = ErrCode Text
                deriving (Eq, Show, IsString, Semigroup, Monoid) via Text

data FlError where
  KnownError ::IsFlError e => e -> FlError
  FlException ::Exception e => e -> FlError

instance Show FlError where
  show = \case
    KnownError  e -> T.unpack $ "KnownError " <> displayFlError e
    FlException e -> "FlException " <> show e

-- | An instance of `IsFlError` indicates if a given type can be serialised to a general error representation.
class IsFlError e where

  -- | Error code, done for api documentation etc.
  errCode :: e -> ErrCode

  -- | Human friendly error message
  userMessage :: e -> Maybe Text

  -- | HTTP Status 
  httpStatus :: e -> HTTP.Status

  -- | Show the error.
  displayFlError :: e -> Text

  default displayFlError :: Show e => e -> Text
  displayFlError e =  show e <> ": " <> show (errCode e, userMessage e, httpStatus e)

  -- | The logging level for the error.
  errorLogLevel :: e -> Log.Level

  -- | Wrap in FlError
  knownError :: e -> FlError
  knownError = KnownError

-- | Throw an error.
throwFlError :: (IsFlError e, Member (E.Error FlError) r) => e -> Sem r a
throwFlError = E.throw . knownError
