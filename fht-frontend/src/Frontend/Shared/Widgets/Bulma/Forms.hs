{-# LANGUAGE RecursiveDo, BangPatterns #-}

module Frontend.Shared.Widgets.Bulma.Forms
  ( simpleFormInput
  , simpleTextInput
  , simpleTextInputDef
  , textInputValidate
  )
where

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
  traceM "here"
  rec
    let eValidation = RD.tag (validate <$> RD.current dTxtTyped) eValidate
        eErrorMaybe = preview _Left <$> eValidation
        eError      = RD.fforMaybe eErrorMaybe identity

    dValidationMsg <- RD.holdDyn "" eError
    dHasErrors     <- RD.toggle False eError

    let
      dValidateClass = dHasErrors <&> \errs ->
        ("class" RD.=:) $ if errs then "help is-danger" else "help is-success"
      mHelp = Just . Tags.pDynAttr dValidateClass . RD.dynText $ dValidationMsg

    dTxtTyped <- RD.traceDyn "dTxtTyped" <$> simpleTextInput conf mHelp label

  traceM "here2"
  RD.holdDyn (Left "") eValidation
