{-# LANGUAGE RecursiveDo #-}
module Frontend.Garden.Plant
  ( plantCard
  , dispPlants
  , module Data.Garden.Plant
  )
where

import           Frontend.Shared.Widgets.Bulma  ( sectionContainer
                                                , tileSection
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Elements            ( emptyEl )
import           Protolude
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as RD
import           Data.Garden.Plant
import           Lib.Reflex.Clicks              ( clickEventWith )

-- | Display a plant as a Bulma card. Events are fired on each click.
plantCard
  :: (RD.DomBuilder t m, RD.PostBuild t m)
  => RD.Dynamic t (Maybe Plant)
  -> Plant
  -> m (RD.Event t Plant) -- ^ Event with clicked plant.
plantCard dSelected plant@Plant {..} = clickEventWith plant $ fst <$> plantTile
 where
  plantTile =
    RD.elClass' "div" "tile is-parent"
      . RD.elDynAttr "article" prodAttrs -- "tile is-child notification is-info"
      $ do
          RD.elClass "p" "title" $ RD.text _pName
          when (isJust _pDesc)  desc
          when (isJust _pImage) image
  image     = RD.elClass "figure" "image is-4by3" $ emptyEl "img" imgAttrs
  imgAttrs  = "src" =: fromMaybe "" _pImage
  desc      = subtitle . RD.text $ fromMaybe "" _pDesc
  subtitle  = RD.elClass "p" "subtitle"
  prodAttrs = dSelected <&> \p -> if p == Just plant
    then prodClass <> ("style" =: "background-color: cadetblue;")
    else prodClass
  prodClass = "class" =: "tile is-child notification is-info"

-- | Display a list of dynamic plants.
dispPlants
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => Maybe Plant -- ^ An optional, initially selected plant.
  -> RD.Dynamic t [Plant] -- ^ Current list of plants 
  -> m ()
dispPlants mInitSelected dPlants = sectionContainer . tileSection $ do
  rec dSelectedPlant <- RD.holdDyn mInitSelected (Just <$> ePlant)
      let dePlants' :: RD.Dynamic t (m [RD.Event t Plant])
          dePlants' = mapM (plantCard dSelectedPlant) <$> dPlants
      ePlants <- fmap RD.leftmost <$> RD.dyn dePlants'
      ePlant  <- RD.switchHold RD.never ePlants
  pure ()

