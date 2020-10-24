module Lib.Reflex.Elements
  ( emptyEl
  )
where

import qualified Reflex.Dom                    as RD
import           Protolude

emptyEl :: RD.DomBuilder t m => Text -> Map Text Text -> m ()
emptyEl el attrs = RD.elAttr el attrs $ RD.text ""
