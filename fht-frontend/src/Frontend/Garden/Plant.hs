{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Garden.Plant
  ( plantCard
  , dispPlants
  , plantMaintenance
  , module Data.Garden.Plant
  )
where

import qualified Data.Map                      as M
import           Lib.Reflex.Buttons
import           Reflex.Bulmex.Modal            ( modal )
import           Control.Lens
import "reflex-dom-helpers" Reflex.Tags        as Tags
import           Frontend.Shared.Widgets.Bulma  ( sectionContainer
                                                , tileSection
                                                , mkSection
                                                )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Elements            ( emptyEl )
import           Protolude
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as RD
import           Data.Garden.Plant
import           Lib.Reflex.Clicks              ( clickEvents
                                                , clickEventWith
                                                , ClickType(DoubleClick)
                                                )

-- | Display a plant as a Bulma card. Events are fired on each click.
plantCard
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t (Maybe FullPlantData) -- ^ A dynamic representing the currently selected plant.
  -> FullPlantData -- ^ Plant to display
  -> m (RD.Event t FullPlantData) -- ^ Event with clicked plant.
plantCard dSelected fpd@(FullPlantData plant@Plant {..} statuses) = do
  eClickType <- clickEvents $ fst <$> plantTile
  -- for any kind of click, we do want this plant to be selected. 
  let eSelected  = eClickType $> fpd
      -- and we want to display the modal if the user has double clicked.
      eShowModal = RD.ffilter (== DoubleClick) eClickType $> ()
  modal eShowModal $ plantMaintenance dSelected
  pure eSelected
 where
  plantTile =
    RD.elClass' "div" "tile is-parent"
      . RD.elDynAttr "article" prodAttrs -- "tile is-child notification is-info"
      $ do
          duesIndicator containsDues'
          RD.elClass "p" "title" $ RD.text _pName
          when (isJust _pDesc)  desc
          when (isJust _pImage) image
  image     = RD.elClass "figure" "image is-4by3" $ emptyEl "img" imgAttrs
  imgAttrs  = "src" =: fromMaybe "" _pImage
  desc      = subtitle . RD.text $ fromMaybe "" _pDesc
  subtitle  = RD.elClass "p" "subtitle"
  prodAttrs = dSelected <&> \fpd' -> if (_fpdPlant <$> fpd') == Just plant
    then prodClass <> ("style" =: "background-color: cadetblue;")
    else prodClass
  prodClass     = "class" =: "tile is-child notification is-info"
  -- we want to display specialised styles for plants that contain dues.
  containsDues' = containsDues statuses

duesIndicator :: RD.DomBuilder t m => Bool -> m ()
duesIndicator containsDues'
  | containsDues' = spanI "icon has-text-warning" "fas fa-exclamation-triangle"
  | otherwise     = spanI "icon has-text-success" "fas fa-check-square"
 where
  spanI sClass' iClass' =
    Tags.spanClass sClass' . Tags.iClass iClass' $ RD.text ""


-- | Display a list of dynamic plants.
dispPlants
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => Maybe FullPlantData -- ^ An optional, initially selected plant.
  -> RD.Dynamic t (FullPlantData -> Bool) -- ^ Filter plants
  -> RD.Dynamic t [FullPlantData] -- ^ Current list of plants 
  -> m (RD.Event t FullPlantData) -- ^ Event with the selected plant.
dispPlants mInitSelected dFilter dAllPlants =
  sectionContainer . tileSection $ do
    rec dSelectedPlant <- RD.holdDyn mInitSelected (Just <$> ePlant)
        let dePlants' :: RD.Dynamic t (m [RD.Event t FullPlantData])
            dePlants' = mapM (plantCard dSelectedPlant) <$> dPlantsFiltered
        ePlants <- fmap RD.leftmost <$> RD.dyn dePlants'
        ePlant  <- RD.switchHold RD.never ePlants
    pure ePlant
  where dPlantsFiltered = filter <$> dFilter <*> dAllPlants

-- | Display the plant maintenance section 
-- Requires a dynamic indicating the currently selected plant: can be nothing if no plant is selected.
plantMaintenance
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t (Maybe FullPlantData)
  -> m (RD.Event t [MaintenanceType]) -- ^ Event indicating which maintenance log(s) the user has marked completed.
plantMaintenance dSelected = do
  deSelected <- showSelection
  RD.switchHold RD.never deSelected
 where
  showSelection = RD.dyn $ dSelected <&> \case
    Nothing ->
      Tags.divClass "box"
        .  Tags.h2Class "subtitle"
        $  RD.text "Select a plant to start."
        $> mempty
    Just fpd@FullPlantData {..} -> Tags.divClass "box" $ do
      Tags.h1Class "title" . RD.text $ fpd ^. fpdPlant . pName
      Tags.h2Class "subtitle" . RD.text $ maintenanceNeededText -- fpd ^. fpdPlant . pName
      displayMaints _fpdMStatuses
     where
      hasPending = containsDues _fpdMStatuses
      maintenanceNeededText =
        if hasPending then "Needs maintenance." else "All good!"

displayMaints
  :: forall t m
   . (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m, RD.PostBuild t m)
  => MaintenanceStatuses
  -> m (RD.Event t [MaintenanceType])
displayMaints maintMap
  | not (null maints) = do
    dSelected <- multiSelect
    -- todo: remove, also requires RD.PostBuild t m 
    RD.dynText (show <$> dSelected)
    eDone <- doneButton
    let bSelected = RD.current dSelected
        eSelected = RD.tag bSelected eDone
    pure eSelected
  | otherwise = do
    Tags.divClass "is-success" . RD.text $ "No required maintenances found."
    pure $ RD.never $> []
 where
  multiSelect =
    Tags.divClass "select is-multiple" . Tags.selectAttr selectAttrs $ do
      eToggled <- mapM dispMaint maints
      RD.foldDyn toggleMaints [] $ RD.mergeWith mappend eToggled
  maints = M.toList maintMap
  dispMaint (mtype, mstat) =
    let isDue = isJust $ mstat ^? _UnsafeDueBy
    in  clickEventWith [mtype] . fmap fst . Tags.option' $ do
          duesIndicator isDue
          RD.text . show $ mtype
  toggleMaints maints' cur = foldr toggleMaint cur maints'
  -- if a maintenance is part of the list of maintenances, remove it, if it isn't add it.
  toggleMaint maint cur | maint `elem` cur = filter (/= maint) cur
                        | otherwise        = maint : cur
  numMaints   = length maints
  selectAttrs = ("multiple" =: "") <> ("size" =: show numMaints)
  doneButton =
    mkButtonConstTextClass "button is-success" mempty "Mark selected as done"
