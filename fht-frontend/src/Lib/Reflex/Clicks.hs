module Lib.Reflex.Clicks
  ( clickEvent
  , clickEvents
  , clickEvent'
  , clickEvents'
  , clickEventWith
  , clickEventsWith
  , ClickType(..)
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

clickEvents
  :: ( DomBuilder t m
     , HasDomEvent t target 'ClickTag
     , HasDomEvent t target 'DblclickTag
     )
  => m target
  -> m (Event t ClickType)
clickEvents = fmap clickEvents'

clickEvents'
  :: ( Reflex t
     , HasDomEvent t target 'ClickTag
     , HasDomEvent t target 'DblclickTag
     )
  => target
  -> Event t ClickType
clickEvents' target = leftmost
  [clickEvent' target $> SingleClick, doubleClickEvent' target $> DoubleClick]

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent'
  :: (Reflex t, HasDomEvent t target 'ClickTag) => target -> Event t ()
clickEvent' = void . domEvent Click

doubleClickEvent'
  :: (Reflex t, HasDomEvent t target 'DblclickTag) => target -> Event t ()
doubleClickEvent' = void . domEvent Dblclick

clickEventsWith
  :: ( DomBuilder t m
     , HasDomEvent t target 'ClickTag
     , HasDomEvent t target 'DblclickTag
     )
  => (ClickType -> value)
  -> m target
  -> m (Event t value)
clickEventsWith mkValue target = clickEvents target <&> (<&> mkValue)

clickEventWith
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => value
  -> m target
  -> m (Event t value)
clickEventWith value' target = clickEvent target <&> ($> value')

data ClickType = SingleClick | DoubleClick
               deriving (Show, Eq)

