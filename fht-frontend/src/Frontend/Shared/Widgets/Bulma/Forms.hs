{-# LANGUAGE RecursiveDo #-}

module Frontend.Shared.Widgets.Bulma.Forms
  ( simpleFormInput
  , simpleTextInput
  , simpleTextInputDef
  , textInputValidate
  , textInputMaybe
  , textInputNonEmpty
  )
where

import qualified Data.Text                     as T
import           Control.Lens
import           Control.Monad.Fix
import qualified "reflex-dom-helpers" Reflex.Tags
                                               as Tags
import           Protolude
import qualified Reflex.Dom                    as RD


-- $forms
-- Support for Bulma forms. 

simpleFormInput :: RD.DomBuilder t m => Text -> m a -> Maybe (m ()) -> m a
simpleFormInput label ctrl mHelp = Tags.divClass "field" $ do
  Tags.labelClass "label" . RD.text $ label
  c <- Tags.divClass "control" ctrl
  case mHelp of
    Nothing  -> pure ()
    Just hlp -> hlp
  pure c

simpleTextInput
  :: RD.DomBuilder t m
  => RD.InputElementConfig RD.EventResult t (RD.DomBuilderSpace m)
  -> Maybe (m ())
  -> Text
  -> m (RD.Dynamic t Text)
simpleTextInput conf mHelp label =
  RD._inputElement_value <$> simpleFormInput label (RD.inputElement conf) mHelp

simpleTextInputDef :: RD.DomBuilder t m => Text -> m (RD.Dynamic t Text)
simpleTextInputDef = simpleTextInput RD.def Nothing

textInputValidate
  :: (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m, RD.PostBuild t m, Show a)
  => RD.InputElementConfig RD.EventResult t (RD.DomBuilderSpace m)
  -> Text
  -> RD.Event t ()
  -> (Text -> Either Text a)
  -> m (RD.Dynamic t (Either Text a))
textInputValidate conf label eValidate validate = do
  rec
    let eValidation = RD.tag (validate <$> RD.current dInputTxt) eValidate

    dValidationMsg <- RD.holdDyn
      ""
      (fromMaybe "" . preview _Left <$> eValidation)
    dHasErrors <- RD.holdDyn False (isLeft <$> eValidation)

    let
      dValidateClass = dHasErrors <&> \errs ->
        ("class" RD.=:) $ if errs then "help is-danger" else "help is-success"
      mHelp = Just . Tags.pDynAttr dValidateClass . RD.dynText $ dValidationMsg

    dInputTxt <- simpleTextInput conf mHelp label

  RD.holdDyn (Left "") eValidation

textInputMaybe
  :: (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m, RD.PostBuild t m, Show a)
  => RD.InputElementConfig RD.EventResult t (RD.DomBuilderSpace m)
  -> Text
  -> RD.Event t ()
  -> (Text -> Maybe a)
  -> m (RD.Dynamic t (Maybe a))
textInputMaybe conf label eValidate validate = do
  rec let eValidation = RD.tag (validate <$> RD.current dInputTxt) eValidate

      dInputTxt <- simpleTextInput conf Nothing label

  RD.holdDyn Nothing eValidation

textInputNonEmpty
  :: (RD.DomBuilder t m, RD.MonadHold t m, MonadFix m, RD.PostBuild t m)
  => RD.InputElementConfig RD.EventResult t (RD.DomBuilderSpace m)
  -> Text
  -> RD.Event t ()
  -> m (RD.Dynamic t (Maybe Text))
textInputNonEmpty conf label eValidate = textInputMaybe conf
                                                        label
                                                        eValidate
                                                        nonEmptyText
 where
  nonEmptyText (T.strip -> t) | T.null t  = Nothing
                              | otherwise = Just t

