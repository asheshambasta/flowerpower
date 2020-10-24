module Main
  ( main
  )
where

import           Control.Lens
import qualified Data.Map                      as Map
import           Control.Monad.Fix              ( MonadFix )
import           Protolude
import           Control.Monad                  ( void )
import qualified Data.Text                     as T
import           Reflex.Dom                    as RD

main :: IO ()
main = mainWidget $ do
  el "h1" $ text "reflex-stone"
  inputMode <- inputModeDropdown
  dText     <- buttonTextInput inputMode
  clicked   <- stoneButton dText
  cnt       <- foldDyn (+) (0 :: Int) $ 1 <$ clicked
  elClass "p" "result" $ dyn_ $ ffor cnt $ \case
    0 -> text "Go ahead and hit the stone."
    n -> text . T.unwords $ [show n, " heads!"]

data InputMode = Realtime | OnKeypressEnter
               deriving (Eq, Show, Ord)

inputModeDropdown
  :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m)
  => m (Dynamic t InputMode)
inputModeDropdown =
  RD.dropdown Realtime (constDyn ops) def <&> RD._dropdown_value
 where
  ops = Map.fromList [(Realtime, "Realtime"), (OnKeypressEnter, "On enter")]

buttonTextInput
  :: (MonadHold t m, MonadFix m, DomBuilder t m)
  => Dynamic t InputMode
  -> m (Dynamic t T.Text)
buttonTextInput imode = do
  input@RD.InputElement { _inputElement_value = dVal } <-
    RD.inputElement
    $ def
    & (  inputElementConfig_elementConfig
      .  elementConfig_initialAttributes
      .~ ("placeholder" =: "Stone!")
      )

  -- Generate an event whenever Enter is pressed, or released.
  let send = RD.leftmost
        [Just <$> keypress Enter input, Nothing <$ keyup Enter input]

  -- a dynamic with press/up events from the Enter key.
  dSend <- RD.holdDyn Nothing send

  -- zip some dynamics: the input mode dynamic, the Enter key dynamic, and the current value in the text input.
  let dTriple = RD.zipDynWith mkTriple (RD.zipDyn imode dSend) dVal

  fmap (view _3) <$> RD.holdUniqDynBy mustPropagate dTriple
 where
  mkTriple (m, e) t = (m, e, t)
  mustPropagate (mode, enterPressed1, txt1) (_, enterPressed2, txt2) =
    case mode of
      Realtime        -> txt1 == txt2 -- fire if text has changed.
      OnKeypressEnter -> enterPressed1 == enterPressed2 -- fire if the enter key's state was modified.

stoneButton
  :: (MonadHold t m, MonadFix m, DomBuilder t m, PostBuild t m)
  => Dynamic t T.Text
  -> m (Event t ())
stoneButton dText = do
  let attr = "style" =: "font-size: 200%;"
  clickEvent $ elAttr' "button" attr (RD.dynText dText)

-- | Get the click event on an element
--
-- Use as:
--   clickEvent $ el' "a" ...
clickEvent
  :: (DomBuilder t m, HasDomEvent t target 'ClickTag)
  => m (target, a)
  -> m (Event t ())
clickEvent = fmap (void . domEvent Click . fst)
