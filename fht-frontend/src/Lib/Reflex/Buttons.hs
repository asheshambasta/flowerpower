module Lib.Reflex.Buttons
  ( mkButton
  , mkButtonConstTextClass
  , mkButtonConstText
  , mkButtonDynClass
  , mkButtonDynClassToggle
  , Class(..)
  )
where

import qualified Data.Map                      as M
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

mkButtonDynClass
  :: (RD.DomBuilder t m, RD.PostBuild t m)
  => RD.Dynamic t Text
  -> m b
  -> m (RD.Event t ())
mkButtonDynClass dClass txt =
  clickEvent $ fst <$> RD.elDynAttr' "button" attrs txt
  where attrs = M.singleton "class" <$> dClass

newtype Class (ctype :: Symbol) = Class Text
                                deriving stock (Show, Eq)
                                deriving IsString via Text

-- | Create a button with a dynamic indicating if the button should be active or not. 
mkButtonDynClassToggle
  :: (RD.DomBuilder t m, RD.PostBuild t m)
  => RD.Dynamic t Bool
  -> Class "Active" -- ^ Active class 
  -> Class "Inactive" -- ^ Inactive class
  -> m b
  -> m (RD.Event t ())
mkButtonDynClassToggle dIsActive (Class activeClass) (Class inactiveClass) =
  mkButtonDynClass dClass
  where dClass = dIsActive <&> \b -> if b then activeClass else inactiveClass
