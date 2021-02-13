{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Garden.Plant
  ( plantCard
  , dispPlants
  , maintenanceModal
  , addPlantModal
  , PlantSelectResult(..)
  , _AddLogsNow
  , _DeletePlant
  , _EditPlant
  , _SelectPlant
  , module Data.Garden.Plant
  )
where

import           Data.Time                      ( Day
                                                , getCurrentTime
                                                )
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
import           Reflex.Dom
import           Data.Garden.Plant
import           Lib.Reflex.Clicks              ( clickEvents
                                                , clickEventWith
                                                , ClickType(SingleClick)
                                                )
import "bulmex"  Reflex.Bulmex.Modal            ( modal
                                                , modalClose
                                                )
import qualified "bulmex" Reflex.Bulmex.Tag.Bulma
                                               as BTags
import "reflex-dom-helpers" Reflex.Tags        as Tags

data PlantSelectResult = AddLogsNow PlantId [MaintenanceLog]
                      | DeletePlant PlantId
                      | EditPlant Plant
                      | SelectPlant FullPlantData
                      deriving (Eq, Show)

makePrisms ''PlantSelectResult

-- | Display a plant as a Bulma card. Events are fired on each click.
plantCard
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO m)
  => Dynamic t (Maybe FullPlantData) -- ^ A dynamic representing the currently selected plant.
  -> FullPlantData -- ^ Plant to display
  -> m (Event t PlantSelectResult) -- ^ Event with clicked plant.
plantCard dSelected fpd@(FullPlantData plant@Plant {..} statuses) =
  Tags.divClass "column is-narrow" $ do
    eClickType <- clickEvents $ fst <$> plantTile
    -- for any kind of click, we do want this plant to be selected. 
    let eSelected  = eClickType $> fpd
        -- and we want to display the modal if the user has double clicked.
        eShowModal = ffilter (== SingleClick) eClickType $> ()
    eModalResult <- fst <$> modalClose eShowModal (maintenanceModal dSelected)
    pure $ leftmost [SelectPlant <$> eSelected, eModalResult]
 where
  plantTile =
    elClass' "div" "tile is-parent"
      . Tags.divClass "box"
      . elDynAttr "article" prodAttrs -- "tile is-child notification is-info"
      $ do
          duesIndicator containsDues'
          elClass "p" "title" $ text _pName
          when (isJust _pDesc)  desc
          when (isJust _pImage) image
  image     = elClass "figure" "image is-4by3" $ emptyEl "img" imgAttrs
  imgAttrs  = "src" =: fromMaybe "" _pImage
  desc      = subtitle . text $ fromMaybe "" _pDesc
  subtitle  = elClass "p" "subtitle"
  prodAttrs = dSelected <&> \fpd' -> if (_fpdPlant <$> fpd') == Just plant
    then prodClass <> ("style" =: "background-color: cadetblue;")
    else prodClass
  prodClass     = "class" =: "tile is-child notification is-info"
  -- we want to display specialised styles for plants that contain dues.
  containsDues' = containsDues statuses

addPlantModal
  :: forall a t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t ()
  -> Event t (Maybe Plant)
  -> (Dynamic t (Either Text Plant) -> Event t () -> m (Event t a))
  -> m (Event t a)
addPlantModal eModal eSelected callback =
  fmap fst . modalClose eModal . box $ do

    rec
      dName        <- nameInput eSaveClicked
      dDesc        <- descInput eSaveClicked
      dImage       <- imageInput eSaveClicked
      dTimePlanted <- timePlantedInput eSaveClicked
      dPlantId     <- holdDyn Nothing (fmap _pId <$> eSelected)
      dMaints <- maintButtons $ maybe mempty (view pMaintenances) <$> eSelected

      let dEitherPlant = do
            eName       <- dName
            plantId     <- dPlantId
            _pDesc      <- dDesc
            _pImage     <- dImage
            maints      <- M.toList <$> dMaints
            eDayPlanted <- dTimePlanted
            pure
              $   Plant (fromMaybe 0 plantId)
              <$> eName
              <*> pure _pDesc
              <*> pure _pImage
              <*> eDayPlanted
              <*> pure (fst <$> maints)
              <*> pure (snd <$> maints)

      eSaveClicked <- mkButtonDynClassToggle (isRight <$> dEitherPlant)
                                             "button is-primary"
                                             "button is-disabled"
                                             (dynText "Save")

    eSaved <- callback dEitherPlant eSaveClicked

    let eClose = ffilter isRight (tag (current dEitherPlant) eSaved) $> ()

    pure (eSaved, eClose)
 where
  imageInput =
    let settings = def & inputElementConfig_setValue .~ eImage
        eImage   = maybe "" (fromMaybe "" . _pImage) <$> eSelected
    in  Bf.textInputNonEmpty settings "Image URL"
  timePlantedInput eSave =
    let settings     = def & inputElementConfig_setValue .~ eTimePlanted
        eTimePlanted = maybe "" (show . _pDayPlanted) <$> eSelected
    in  Bf.textInputValidate settings
                             "Date planted (YYYY-MM-DD)"
                             eSave
                             (first T.pack . readEither @Day . T.unpack)
  descInput =
    let settings = def & inputElementConfig_setValue .~ eDesc
        eDesc    = maybe "" (fromMaybe "" . _pDesc) <$> eSelected
    in  Bf.textInputNonEmpty settings "Description"
  nameInput eSave =
    let settings = def & inputElementConfig_setValue .~ eName
        eName    = maybe "" _pName <$> eSelected
    in  Bf.textInputValidate settings "Name" eSave validateName
  box = fmap snd . Bw.box (text "Add/Edit")
  validateName txt | T.null txt = Left "Name cannot be empty"
                   | otherwise  = Right txt

-- | Display a set of maintenance buttons, the event indicates the button that was pressed.
maintButtons
  :: forall t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => Event t Maintenances
  -> m (Dynamic t Maintenances)
maintButtons eInitialMaints = Tags.divClass "buttons" $ do
  rec dMaints     <- foldDyn (M.alter rotate') initMaints eMaintType
      eMaintType  <- mapM (mkButton' dMaints) allMTypes <&> leftmost
      bInitMaints <- hold mempty eInitialMaints
      initMaints  <- sample bInitMaints
  pure dMaints
 where
  mkButton' dMaints mt =
    let dFreqMaybe  = M.lookup mt <$> dMaints
        dIsSelected = isJust <$> dFreqMaybe
        dButtonTxt  = dynText $ dMaints <&> displayMtWithFreq mt
    in  ($> mt)
          <$> mkButtonDynClassToggle dIsSelected
                                     "button is-primary is-light"
                                     "button"
                                     dButtonTxt

  allMTypes = enumFromTo @MaintenanceType minBound maxBound
  rotate' Nothing = Just minBound
  rotate' (Just mf) | mf == maxBound = Nothing
                    | otherwise      = Just $ succ mf

displayMtWithFreq
  :: MaintenanceType -> Map MaintenanceType MaintenanceFreq -> Text
displayMtWithFreq mt maints = case M.lookup mt maints of
  Nothing -> show mt <> " (+)"
  Just mf -> show mt <> " (" <> T.singleton (T.head . show $ mf) <> ")"

duesIndicator :: DomBuilder t m => Bool -> m ()
duesIndicator containsDues'
  | containsDues' = Bw.spanIEmpty "icon has-text-warning"
                                  "fas fa-exclamation-triangle"
  | otherwise = Bw.spanIEmpty "icon has-text-success" "fas fa-check-square"

-- | Display a list of dynamic plants.
dispPlants
  :: forall t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO m)
  => Maybe FullPlantData -- ^ An optional, initially selected plant.
  -> Dynamic t (FullPlantData -> Bool) -- ^ Filter plants
  -> Dynamic t [FullPlantData] -- ^ Current list of plants 
  -> m (Event t PlantSelectResult) -- ^ Event with the selected plant.
dispPlants mInitSelected dFilter dAllPlants =
  Bw.mkSection
    . Tags.divClass "columns is-mobile is-multiline is-vcentered"
    $ do
        rec
          dSelectedPlant <- holdDyn mInitSelected
                                    (preview _SelectPlant <$> ePlant)
          ePlants <- fmap leftmost
            <$> dyn (dePlants' dSelectedPlant dFilter dAllPlants)
          ePlant <- switchHold never ePlants
        pure ePlant

dePlants'
  :: forall t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO m)
  => Dynamic t (Maybe FullPlantData)
  -> Dynamic t (FullPlantData -> Bool) -- ^ Filter plants
  -> Dynamic t [FullPlantData]
  -> Dynamic t (m [Event t PlantSelectResult])
dePlants' dSelectedPlant dFilter dAllPlants =
  mapM (plantCard dSelectedPlant) <$> dPlantsFiltered
  where dPlantsFiltered = filter <$> dFilter <*> dAllPlants

-- | Display the plant maintenance section 
-- Requires a dynamic indicating the currently selected plant: can be nothing if no plant is selected.
maintenanceModal
  :: forall t m
   . (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m, MonadIO m)
  => Dynamic t (Maybe FullPlantData)
  -> m (Event t PlantSelectResult, Event t ()) -- ^ Event indicating which maintenance log(s) the user has marked completed.
maintenanceModal dSelected = Tags.divClass "box" $ do
  eEditDelete <- Tags.divClass "buttons" editDeleteButtons
  deSelected  <- showSelection
  eAddLogs    <- switchHold never deSelected
  pure (leftmost [eAddLogs, eEditDelete], eEditDelete $> ())
 where
  editDeleteButtons = BTags.buttons $ do
     -- edit & delete buttons button 
    eEditClick   <- Bw.faButton "fa fa-pencil"
    eDeleteClick <- Bw.faButton "fa fa-trash"

    let bMaybeId    = current $ fmap (_pId . _fpdPlant) <$> dSelected
        bMaybePlant = current $ fmap _fpdPlant <$> dSelected
        eEdit       = EditPlant <$> tagMaybe bMaybePlant eEditClick
        eDelete     = DeletePlant <$> tagMaybe bMaybeId eDeleteClick
    pure . leftmost $ [eEdit, eDelete]
  showSelection = dyn $ dSelected <&> \fpd ->
    let mStatuses  = fromMaybe mempty $ fpd ^? _Just . fpdMStatuses
        mId        = fpd ^? _Just . fpdPlant . pId
        hasPending = containsDues mStatuses
        maintenanceNeededText =
          if hasPending then "Needs maintenance." else "All good!"
        title    = text . fromMaybe "" $ fpd ^? _Just . fpdPlant . pName
        subtitle = text maintenanceNeededText -- fpd ^. fpdPlant . pName
    in  do
          Bw.box title subtitle
          case mId of
            Just id -> displayMaints id mStatuses
            Nothing -> pure never

displayMaints
  :: forall t m
   . (DomBuilder t m, MonadHold t m, MonadFix m, MonadIO m)
  => PlantId
  -> MaintenanceStatuses
  -> m (Event t PlantSelectResult)
displayMaints id (M.toList -> maints)
  | not (null dueMaints) = do
    dSelected <- multiSelect
    eDone     <- doneButton
    time      <- liftIO getCurrentTime
    let bSelected = current dSelected
        eSelected = tag bSelected eDone
        eLog      = eSelected <&> fmap (`MaintenanceLog` time)
    pure $ AddLogsNow id <$> eLog
  | otherwise = do
    Tags.divClass "is-success" . text $ "No required maintenances found."
    pure never
 where
  dueMaints   = filter (isDue . snd) maints
  multiSelect = halfArea $ do
    eToggled <- mapM dispMaint dueMaints
    foldDyn toggleMaints [] $ mergeWith mappend eToggled
  -- maints = M.toList maintMap
  dispMaint (mtype, _) =
    clickEventWith [mtype] . fmap fst . mkBox . text . show $ mtype
  mkBox ma = Tags.divClass "column" $ do
    checkbox' <-
      elClass "label" "checkbox" . elAttr' "input" ("type" =: "checkbox") $ text
        ""
    ma
    pure checkbox'
  toggleMaints maints' cur = foldr toggleMaint cur maints'
  -- if a maintenance is part of the list of maintenances, remove it, if it isn't add it.
  toggleMaint maint cur | maint `elem` cur = filter (/= maint) cur
                        | otherwise        = maint : cur
  doneButton =
    mkButtonConstTextClass "button is-success" mempty "Mark selected as done"

halfArea :: DomBuilder t m => m a -> m a
halfArea =
  Tags.divClass "columns" . Tags.divClass "column is-half" . Tags.divClass
    "columns"
