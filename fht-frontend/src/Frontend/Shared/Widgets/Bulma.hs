module Frontend.Shared.Widgets.Bulma
  ( sectionContainer
  , tileSection
  , mkSection
  , spanI
  , spanIEmpty
  , box
  , faButton
  )
where

import           Lib.Reflex.Clicks              ( clickEvent )
import qualified "reflex-dom-helpers" Reflex.Tags
                                               as Tags
import           Protolude
import qualified Reflex.Dom                    as RD

-- | https://bulma.io/documentation/layout/container/
sectionContainer :: RD.DomBuilder t m => m a -> m a
sectionContainer = mkSection . Tags.divClass "container"

tileSection :: RD.DomBuilder t m => m a -> m a
tileSection =
  Tags.divClass "tile is-ancestor"
    . Tags.divClass "tile is-vertical is-8"
    . Tags.divClass "tile"

mkSection :: RD.DomBuilder t m => m a -> m a
mkSection = Tags.sectionClass "section"

spanI :: RD.DomBuilder t m => Text -> Text -> m a -> m a
spanI sClass' iClass' = Tags.spanClass sClass' . Tags.iClass iClass'

spanIEmpty :: RD.DomBuilder t m => Text -> Text -> m ()
spanIEmpty sClass' iClass' =
  Tags.spanClass sClass' . Tags.iClass iClass' $ RD.text ""

box :: RD.DomBuilder t m => m a -> m b -> m (a, b)
box title subtitle = Tags.divClass "box" $ do
  t <- Tags.h1Class "title" title
  s <- Tags.h2Class "subtitle" subtitle
  pure (t, s)

-- | Create a button with a font-awesome icon.
faButton :: RD.DomBuilder t m => Text -> m (RD.Event t ())
faButton txt = clickEvent $ fst <$> RD.elClass'
  "button"
  "button"
  (spanIEmpty "icon is-small" txt)

