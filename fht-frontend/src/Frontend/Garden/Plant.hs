{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Garden.Plant
  ( plantCard
  , dispPlants
  , plantMaintenance
  , addPlantModal
  , MaintBoxMode(..)
  , module Data.Garden.Plant
  )
where

import           Data.Time                      ( Day )
import qualified Data.Text                     as T
import qualified Data.Map                      as M
import           Lib.Reflex.Buttons
import           Control.Lens
import qualified Frontend.Shared.Widgets.Bulma as Bw
import qualified Frontend.Shared.Widgets.Bulma.Forms
                                               as Bf
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Elements            ( emptyEl )
import           Protolude               hiding ( to )
import           Reflex.Dom                     ( (=:) )
import qualified Reflex.Dom                    as RD
import           Data.Garden.Plant
import           Lib.Reflex.Clicks              ( clickEvents
                                                , clickEventWith
                                                , ClickType(DoubleClick)
                                                )
import "bulmex"  Reflex.Bulmex.Modal            ( modal
                                                , modalClose
                                                )
import qualified "bulmex" Reflex.Bulmex.Tag.Bulma
                                               as BTags
import "reflex-dom-helpers" Reflex.Tags        as Tags

data MaintBoxMode = Modal | Summary
                  deriving (Eq, Show)

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
  modal eShowModal $ plantMaintenance dSelected Modal
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

addPlantModal
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Event t ()
  -> (  RD.Dynamic t (Either Text Plant)
     -> RD.Event t ()
     -> m (RD.Event t a)
     )
  -> m (RD.Event t a)
addPlantModal eModal callback = fmap fst . modalClose eModal . box $ do
  rec dName <- Bf.textInputValidate RD.def "Name" eSaveClicked validateName
      dDesc        <- Bf.textInputNonEmpty RD.def "Description" eSaveClicked
      dImage       <- Bf.textInputNonEmpty RD.def "Image URL" eSaveClicked
      dTimePlanted <- Bf.textInputValidate
        RD.def
        "Date planted (YYYY-MM-DD)"
        eSaveClicked
        (first T.pack . readEither @Day . T.unpack)
      dMaints <- maintButtons

      let dEitherPlant = do
            eName       <- dName
            _pDesc      <- dDesc
            _pImage     <- dImage
            maints      <- M.toList <$> dMaints
            eDayPlanted <- dTimePlanted
            pure
              $   Plant 0
              <$> eName
              <*> pure _pDesc
              <*> pure _pImage
              <*> eDayPlanted
              <*> pure (fst <$> maints)
              <*> pure (snd <$> maints)

      eSaveClicked <- mkButtonDynClassToggle (isRight <$> dEitherPlant)
                                             "button is-primary"
                                             "button is-disabled"
                                             (RD.dynText "Save")

  eSaved <- callback dEitherPlant eSaveClicked

  let eClose = RD.ffilter isRight (RD.updated dEitherPlant) $> ()

  pure (eSaved, eClose)
 where
  box = fmap snd . Bw.box (RD.text "Add a new plant")
  validateName txt | T.null txt = Left "Name cannot be empty"
                   | otherwise  = Right txt

-- | Display a set of maintenance buttons, the event indicates the button that was pressed.
maintButtons
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => m (RD.Dynamic t (Map MaintenanceType MaintenanceFreq))
maintButtons = Tags.divClass "buttons" $ do
  rec eMaintType <- mapM (mkButton' dMaints) allStats <&> RD.leftmost
      dMaints    <- RD.foldDyn (M.alter rotate') mempty eMaintType
  pure dMaints
 where
  mkButton' dMaints mt =
    let dFreqMaybe  = M.lookup mt <$> dMaints
        dIsSelected = isJust <$> dFreqMaybe
        dButtonTxt  = RD.dynText $ dMaints <&> displayMtWithFreq mt
    in  ($> mt)
          <$> mkButtonDynClassToggle dIsSelected
                                     "button is-primary is-light"
                                     "button"
                                     dButtonTxt

  allStats = enumFromTo @MaintenanceType minBound maxBound
  rotate' Nothing = Just minBound
  rotate' (Just mf) | mf == maxBound = Nothing
                    | otherwise      = Just $ succ mf

displayMtWithFreq
  :: MaintenanceType -> Map MaintenanceType MaintenanceFreq -> Text
displayMtWithFreq mt maints = case M.lookup mt maints of
  Nothing -> show mt <> " (+)"
  Just mf -> show mt <> " (" <> T.singleton (T.head . show $ mf) <> ")"

duesIndicator :: RD.DomBuilder t m => Bool -> m ()
duesIndicator containsDues'
  | containsDues' = Bw.spanIEmpty "icon has-text-warning"
                                  "fas fa-exclamation-triangle"
  | otherwise = Bw.spanIEmpty "icon has-text-success" "fas fa-check-square"

-- | Display a list of dynamic plants.
dispPlants
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => Maybe FullPlantData -- ^ An optional, initially selected plant.
  -> RD.Dynamic t (FullPlantData -> Bool) -- ^ Filter plants
  -> RD.Dynamic t [FullPlantData] -- ^ Current list of plants 
  -> m (RD.Event t FullPlantData) -- ^ Event with the selected plant.
dispPlants mInitSelected dFilter dAllPlants =
  Bw.sectionContainer . Bw.tileSection $ do
    rec dSelectedPlant <- RD.holdDyn mInitSelected (Just <$> ePlant)
        ePlants        <- fmap RD.leftmost
          <$> RD.dyn (dePlants' dSelectedPlant dFilter dAllPlants)
        ePlant <- RD.switchHold RD.never ePlants
    pure ePlant

dePlants'
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t (Maybe FullPlantData)
  -> RD.Dynamic t (FullPlantData -> Bool) -- ^ Filter plants
  -> RD.Dynamic t [FullPlantData]
  -> RD.Dynamic t (m [RD.Event t FullPlantData])
dePlants' dSelectedPlant dFilter dAllPlants =
  mapM (plantCard dSelectedPlant) <$> dPlantsFiltered
  where dPlantsFiltered = filter <$> dFilter <*> dAllPlants

-- | Display the plant maintenance section 
-- Requires a dynamic indicating the currently selected plant: can be nothing if no plant is selected.
plantMaintenance
  :: forall t m
   . (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t (Maybe FullPlantData)
  -> MaintBoxMode
  -> m (RD.Event t [MaintenanceType]) -- ^ Event indicating which maintenance log(s) the user has marked completed.
plantMaintenance dSelected mode = do
  deSelected <- showSelection
  RD.switchHold RD.never deSelected
 where
  showSelection = RD.dyn $ dSelected <&> \case
    Nothing ->
      Bw.box (RD.text "Welcome to Flowerpower!")
             (RD.text "Select a plant to start.")
        $> mempty
    Just fpd@FullPlantData {..} -> Tags.divClass "box" $ do
      Bw.box title subtitle
      case mode of
        Modal   -> displayMaints _fpdMStatuses
        Summary -> pure RD.never
     where
      hasPending = containsDues _fpdMStatuses
      maintenanceNeededText =
        if hasPending then "Needs maintenance." else "All good!"
      title = do
        RD.text $ fpd ^. fpdPlant . pName
        BTags.buttons $ do
          -- edit button 
          Bw.faButton "fa fa-sliders"
          Bw.faButton "fa fa-trash"
      subtitle = RD.text maintenanceNeededText -- fpd ^. fpdPlant . pName

displayMaints
  :: forall t m
   . (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m)
  => MaintenanceStatuses
  -> m (RD.Event t [MaintenanceType])
displayMaints (M.toList -> maints)
  | not (null maints) = do
    dSelected <- multiSelect
    eDone     <- doneButton
    let bSelected = RD.current dSelected
        eSelected = RD.tag bSelected eDone
    pure eSelected
  | otherwise = do
    Tags.divClass "is-success" . RD.text $ "No required maintenances found."
    pure $ RD.never $> []
 where
  dueMaints   = filter (isDue . snd) maints
  multiSelect = halfArea $ do
    eToggled <- mapM dispMaint dueMaints
    RD.foldDyn toggleMaints [] $ RD.mergeWith mappend eToggled
  -- maints = M.toList maintMap
  dispMaint (mtype, _) =
    clickEventWith [mtype] . fmap fst . mkBox . RD.text . show $ mtype
  mkBox ma = Tags.divClass "column" $ do
    el <-
      RD.elClass "label" "checkbox"
      . RD.elAttr' "input" ("type" =: "checkbox")
      $ RD.text ""
    ma
    pure el
  toggleMaints maints' cur = foldr toggleMaint cur maints'
  -- if a maintenance is part of the list of maintenances, remove it, if it isn't add it.
  toggleMaint maint cur | maint `elem` cur = filter (/= maint) cur
                        | otherwise        = maint : cur
  doneButton =
    mkButtonConstTextClass "button is-success" mempty "Mark selected as done"

halfArea :: RD.DomBuilder t m => m a -> m a
halfArea =
  Tags.divClass "columns" . Tags.divClass "column is-half" . Tags.divClass
    "columns"
