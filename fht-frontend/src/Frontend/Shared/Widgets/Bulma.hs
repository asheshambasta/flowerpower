module Frontend.Shared.Widgets.Bulma
  ( sectionContainer
  )
where

import           Protolude
import qualified Reflex.Dom                    as Dom

-- | https://bulma.io/documentation/layout/container/
sectionContainer :: Dom.DomBuilder t m => m a -> m a
sectionContainer =
  Dom.elClass "section" "section" . Dom.elClass "div" "container"
