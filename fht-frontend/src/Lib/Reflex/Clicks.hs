module Lib.Reflex.Clicks
  ( clickEvent
  , clickEvent'
  , clickEventWith
  )
where

import           Protolude
import           Reflex.Dom

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => m target
  -> m (Event t ())
clickEvent = fmap clickEvent'

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent'
  :: (Reflex t, HasDomEvent t target 'ClickTag) => target -> Event t ()
clickEvent' = void . domEvent Click

clickEventWith
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => value
  -> m target
  -> m (Event t value)
clickEventWith value' target = clickEvent target <&> ($> value')
