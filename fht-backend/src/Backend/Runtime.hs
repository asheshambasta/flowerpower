module Backend.Runtime
  ( module DB
  , module L
  , Runtime(..)
  , confP
  )
where

import "prelude-polysemy" Prelude.Control.Log  as L
import "dbstorage-polysemy" Database.Runtime   as DB
import qualified Options.Applicative           as A

data Conf = Conf
  { _cDBConf          :: DB.DBConf
  , _cApplicationName :: Text
  }

-- | The runtime is nothing but a product of materialised values useful for running the application.
data Runtime = Runtime
  { _rDB     :: DB.DBConnPool
  , _rLogger :: L.Logger
  , _rConf   :: Conf
  }

confP :: A.Parser Conf
confP = undefined
