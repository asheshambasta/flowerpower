module Lib.Reflex.Buttons
  ( mkButton
  , mkButtonConstTextClass
  , mkButtonConstText
  )
where

import           Lib.Reflex.Clicks              ( clickEvent )
import           Protolude
import qualified Reflex.Dom                    as RD

mkButton :: RD.DomBuilder t m => Map Text Text -> m b -> m (RD.Event t ())
mkButton attrs txt = clickEvent $ fst <$> RD.elAttr' "button" attrs txt

mkButtonConstText
  :: RD.DomBuilder t m => Map Text Text -> Text -> m (RD.Event t ())
mkButtonConstText attrs = mkButton attrs . RD.text

mkButtonConstTextClass
  :: RD.DomBuilder t m => Text -> Map Text Text -> Text -> m (RD.Event t ())
mkButtonConstTextClass class' attrs =
  mkButtonConstText (attrs <> ("class" RD.=: class'))
