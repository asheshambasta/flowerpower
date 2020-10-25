{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
module Frontend.Nav
  ( top
  , NavSettings(..)
  , Navigation(..)
  , DispMode(..)
  )
where

import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Clicks              ( clickEvent )
import           Lib.Reflex.Buttons             ( mkButtonConstTextClass )
import           Data.Default.Class             ( Default(..) )
import           Control.Lens
import           Protolude
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as RD
import qualified "reflex-dom-helpers" Reflex.Tags
                                               as Tags

data NavSettings t = NavSettings
  { _navTotalPlants :: RD.Dynamic t Int -- ^ Current total number of plants.
  , _navInitial     :: Navigation
  }

data DispMode = ShowAll
              | ShowDueBy
              deriving (Show, Eq)

data Navigation = Search Text
                | AddNew
                | ChangeDisplay DispMode
                deriving (Eq, Show)

instance Default Navigation where
  def = Search ""

makePrisms ''Navigation

-- | Create the top level navigation bar.
top
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => NavSettings t
  -> m (RD.Dynamic t Navigation)
top NavSettings {..} =
  Tags.navClass "level" $ navTopLeft _navTotalPlants >>= navTopRight _navInitial

navTopRight
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => Navigation
  -> RD.Event t Navigation
  -> m (RD.Dynamic t Navigation)
navTopRight initial eNavLeft = Tags.divClass "level-right" $ do
  rec
    -- event from the right of the navbar.
    let eNavRight = RD.leftmost
          [ eShowAll $> ChangeDisplay ShowAll
          , eShowDueBy $> ChangeDisplay ShowDueBy
          , eAdd $> AddNew
          ]
        -- the "sum" of all events from the navbar.
        eNav = RD.leftmost [eNavLeft, eNavRight]
        dAll = dOn $ ChangeDisplay ShowAll
        dDue = dOn $ ChangeDisplay ShowDueBy
        -- this is a hack. Replace with the Bulma equivalent of "disabled" etc.
        dOn t = dNav <&> \case
          t' | t' == t -> "style" =: "color: black;"
          _            -> mempty
    dNav       <- RD.holdDyn initial eNav

    eShowAll   <- levelItemClick . Tags.aDynAttr' dAll $ RD.text "All"
    eShowDueBy <- levelItemClick . Tags.aDynAttr' dDue $ RD.text "Due"

    eAdd <- levelItemP $ mkButtonConstTextClass "button is-success" mempty "Add"

  pure dNav
 where
  levelItemP     = Tags.pClass "level-item"
  levelItemClick = Tags.pClass "level-item" . clickEvent . fmap fst

navTopLeft
  :: (RD.DomBuilder t m, RD.PostBuild t m)
  => RD.Dynamic t Int
  -> m (RD.Event t Navigation)
navTopLeft (fmap show -> totalPlants) = Tags.divClass "level-left" $ do
  -- on the left side, we want to display a total count next to a search input.
  levelItem
    . Tags.pClass "subtitle is-5"
    . RD.el "strong"
    . RD.dynText
    $ totalPlants

  levelItem . Tags.divClass "field has-addons" $ do
    -- search input (dynamic contains the latest value of the text in the search box.)
    dSearch <- Tags.pClass "control" mkInput
    -- search button: each click is an event.
    eClick  <- Tags.pClass "control" mkButton

    let eSearch = RD.tag (RD.current dSearch) eClick
    pure $ Search <$> eSearch
 where
  mkButton = mkButtonConstTextClass "button" mempty "Search"
  mkInput  = RD._inputElement_value <$> RD.inputElement inputSettings
  inputSettings =
    def
      &  RD.inputElementConfig_elementConfig
      .  RD.elementConfig_initialAttributes
      .~ (  "placeholder"
         =: "Search for a plant"
         <> "class"
         =: "input"
         <> "type"
         =: "text"
         )

levelItem :: RD.DomBuilder t m => m a -> m a
levelItem = Tags.divClass "level-item"
