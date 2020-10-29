module Frontend.Shared.Widgets.Bulma
  ( sectionContainer
  , tileSection
  )
where

import           Protolude
import qualified Reflex.Dom                    as RD

-- | https://bulma.io/documentation/layout/container/
sectionContainer :: RD.DomBuilder t m => m a -> m a
sectionContainer =
  RD.elClass "section" "section" . RD.elClass "div" "container"

tileSection :: RD.DomBuilder t m => m a -> m a
tileSection =
  RD.elClass "div" "tile is-ancestor"
    . RD.elClass "div" "tile is-vertical is-8"
    . RD.elClass "div" "tile"
