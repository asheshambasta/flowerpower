{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE NamedFieldPuns #-}
module Exercises.Ex02
  ( runEx02
  )
where

import           Exercises.Shared
import           Lib.Reflex.Buttons             ( mkButtonConstText )
import           Control.Monad.Fix              ( MonadFix )
import           Protolude               hiding ( Product )
import           Reflex.Dom                    as RD

runEx02 :: IO ()
runEx02 = mainWidget $ do
  rec inputs <- dispInputs oeSpend
      let outputs@Outputs { oeSpend } = ex02 inputs
  dispOutputs outputs

dispInputs
  :: (DomBuilder t m, MonadHold t m, MonadFix m, PostBuild t m)
  => Event t Money
  -> m (Inputs t)
dispInputs eSpend = el "div" $ do

  ieCarrot   <- dispProduct (pure def) carrot
  ieCelery   <- dispProduct (pure def) celery
  ieCucumber <- dispProduct (pure def) cucumber
  ieRefund   <- mkButtonConstText mempty "Refund"

  eAddMoney  <- addMoneyButton
  dIbMoney   <- foldDyn ($) 0
    $ mergeWith
        (.)
        [eAddMoney $> (+ 1), eSpend <&> flip (-), ieRefund $> const 0]

  let ibMoney = current dIbMoney
      eChange = RD.tag ibMoney ieRefund

  dispMoney dIbMoney
  dispChange eChange

  pure Inputs { .. }
 where
  addMoneyButton = mkButtonConstText mempty "Add money"
  dispChange eChange = do
    dChange <- RD.holdDyn 0 eChange
    RD.text "Change: "
    RD.dynText $ showMoney <$> dChange

dispOutputs
  :: (PostBuild t m, DomBuilder t m, MonadHold t m) => Outputs t -> m ()
dispOutputs Outputs {..} = el "div" $ do
  dVend   <- RD.holdDyn "" oeVend
  dError  <- RD.holdDyn Nothing oeError
  dChange <- RD.holdDyn 0 oeSpend
  elAttr "span" mempty (RD.dynText $ showMoney <$> dChange)
  elAttr "span" mempty (RD.dynText dVend)
  elAttr "div"  mempty (RD.dynText $ maybe "" show <$> dError)

data Inputs t = Inputs
  { ibMoney    :: Behavior t Money
  , ieCarrot   :: Event t ()
  , ieCelery   :: Event t ()
  , ieCucumber :: Event t ()
  , ieRefund   :: Event t ()
  }

data Outputs t = Outputs
  { oeVend   :: Event t Text
  , oeSpend  :: Event t Money
  , oeChange :: Event t Money
  , oeError  :: Event t (Maybe Error)
  }

data Error = NotEnoughMoney
  deriving (Eq, Ord, Show)

ex02 :: Reflex t => Inputs t -> Outputs t
ex02 Inputs {..} = Outputs { oeVend   = pName . snd <$> eAllowedPurchase
                           , oeSpend  = pCost . snd <$> eAllowedPurchase
                           , oeChange = tag ibMoney ieRefund
                           , ..
                           }
 where
  oeError = leftmost
    [ difference (ePurchaseAttempt $> Just NotEnoughMoney) eAllowedPurchase
    , eAllowedPurchase $> Nothing
    ]
  ePurchaseAttempt =
    leftmost [carrot <$ ieCarrot, celery <$ ieCelery, cucumber <$ ieCucumber]
  -- tuple of the current money in the system, and the attempt to purchase. 
  ePurchaseCurMoney = attach ibMoney ePurchaseAttempt
  eAllowedPurchase  = ffilter (uncurry canBuy) ePurchaseCurMoney
  canBuy money Product { pCost } = pCost <= money
