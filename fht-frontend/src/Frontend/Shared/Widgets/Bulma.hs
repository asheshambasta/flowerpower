module Frontend.Shared.Widgets.Bulma
  ( sectionContainer
  , tileSection
  , mkSection
  )
where

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
