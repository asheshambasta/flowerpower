module Backend.Runtime
  ( module DB
  , module L
  , Runtime(..)
  )
where

import "prelude-polysemy" Prelude.Control.Log  as L
import "dbstorage-polysemy" Database.Conf      as DB

data Conf

-- | The runtime is nothing but a product of materialised values useful for running the application.
data Runtime = Runtime
  { _rDB     :: DB.DBConnPool
  , _rLogger :: L.Logger
  , _rConf   :: Conf
  }
