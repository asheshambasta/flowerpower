{-# LANGUAGE RecursiveDo, NamedFieldPuns, TypeApplications #-}
{-|
Description: Solutions to https://qfpl.io/posts/reflex/basics/exercises/dynamics/index.html
-}
module Exercises.Ex03
  ( runEx03
  )
where

import           Data.Default.Class             ( def )
import qualified Data.Map                      as M
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Buttons             ( mkButtonConstTextClass )
import           Protolude               hiding ( Product )
import qualified Reflex.Dom                    as RD
import           Reflex.Dom                     ( (=:) )
import           Exercises.Shared
import           Exercises.Shared.Widgets       ( mainWidgetWithBulma )
import           Exercises.Shared.Widgets.Bulma ( sectionContainer )

runEx03 :: IO ()
runEx03 = mainWidgetWithBulma $ do
  rec inputs                  <- sectionContainer . dispInputs $ oeVend
      outs@Outputs { oeVend } <- ex03 inputs
  sectionContainer . dispOutputs $ outs

data Inputs t = Inputs
  { idMoney    :: RD.Dynamic t Money
  , idCarrot   :: RD.Dynamic t Stock
  , idCelery   :: RD.Dynamic t Stock
  , idCucumber :: RD.Dynamic t Stock
  , idSelected :: RD.Dynamic t ProductStock
  , ieBuy      :: RD.Event t ()
  , ieRefund   :: RD.Event t ()
  }

data Outputs t = Outputs
  { oeVend   :: RD.Event t Product
  , oeChange :: RD.Event t Money
  , oeError  :: RD.Event t VendingErr
  , odChange :: RD.Dynamic t Money
  , odVend   :: RD.Dynamic t (Maybe Product)
  , odCart   :: RD.Dynamic t (M.Map Product Int)
  }

data VendingErr = InsufficientFunds Money Money | InsufficientStock
                deriving (Eq, Show)

dispInputs
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => RD.Event t Product
  -> m (Inputs t)
dispInputs eVend = do
  RD.elClass "h3" "title" $ RD.text "Choose an item."

  idCarrot   <- mkStock def eCarrot
  idCucumber <- mkStock def eCucumber
  idCelery   <- mkStock def eCelery

  rec

    dispMoney' idMoney

    (eSelectCarrot, eSelectCucumber, eSelectCelery) <- productTiles
      idSelectedProd
      idCarrot
      idCucumber
      idCelery

    (eAddMoney, ieRefund, ieBuy) <- controlButtons

    idMoney                      <- RD.foldDyn ($) 0 $ RD.mergeWith
      (.)
      [eAddMoney $> (+ 1), ieRefund $> const 0, eVend <&> subtract . pCost]

    idSelectedProd <- RD.holdDyn def eSelect

    let eSelect = RD.leftmost [eSelectCarrot, eSelectCucumber, eSelectCelery]
        idSelected =
          idSelectedProd >>= matchProduct idCarrot idCucumber idCelery

  pure Inputs { .. }
 where
  mkStock init = RD.foldDyn ($) init . reduceWith
  reduceWith e = e $> subtract 1
  -- events indicating the product that was bought.
  eCarrot   = RD.ffilter (== carrot) eVend
  eCelery   = RD.ffilter (== celery) eVend
  eCucumber = RD.ffilter (== cucumber) eVend
  -- this is ugly but will do for now.
  matchProduct idCarrot idCucumber idCelery p
    | p == carrot   = ProductStock p <$> idCarrot
    | p == cucumber = ProductStock p <$> idCucumber
    | otherwise     = ProductStock p <$> idCelery
  dispMoney' =
    withinNav . withinNavDiv . withinNavPs "Money inserted" . dispMoney

productTiles
  :: (RD.DomBuilder t m, RD.PostBuild t m, MonadFix m)
  => RD.Dynamic t Product
  -> RD.Dynamic t Stock
  -> RD.Dynamic t Stock
  -> RD.Dynamic t Stock
  -> m
       ( RD.Event t Product
       , RD.Event t Product
       , RD.Event t Product
       )
productTiles dSelected idCarrot idCucumber idCelery =
  RD.elClass "div" "tile is-ancestor"
    . RD.elClass "div" "tile is-vertical is-8"
    . RD.elClass "div" "tile"
    $ do
        eSelectCarrot   <- dispProductStock carrot dSelected idCarrot
        eSelectCucumber <- dispProductStock cucumber dSelected idCucumber
        eSelectCelery   <- dispProductStock celery dSelected idCelery
        pure (eSelectCarrot, eSelectCucumber, eSelectCelery)

controlButtons
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => m (RD.Event t (), RD.Event t (), RD.Event t ())
controlButtons = RD.elClass "div" "buttons" $ do
  eAddMoney' <- mkButtonConstTextClass "button is-info" mempty "Add money"
  ieBuy'     <- mkButtonConstTextClass "button is-success" mempty "Buy"
  ieRefund'  <- mkButtonConstTextClass "button is-danger" mempty "Refund"
  pure (eAddMoney', ieRefund', ieBuy')

dispOutputs
  :: (RD.DomBuilder t m, RD.MonadHold t m, RD.PostBuild t m, MonadFix m)
  => Outputs t
  -> m ()
dispOutputs Outputs {..} =
  void . withinNav $ dispChange >> dispVend >> dispError
 where
  dispChange =
    withinNavDiv $ withinNavPs "Change" (RD.dynText $ showMoney <$> odChange)
  dispVend = withinNavDiv $ withinNavPs
    "Last bought"
    (RD.dynText $ maybe "--" showProduct <$> odVend)
  dispError = do
    dError <- RD.holdDyn "" eError
    withinNavDiv $ withinNavPs "" (RD.dynText dError)
  eError = RD.leftmost [oeVend $> "", oeError <&> show @VendingErr @Text]

withinNav :: RD.DomBuilder t m => m a -> m a
withinNav = RD.elClass "nav" "level"

withinNavDiv :: RD.DomBuilder t m => m a -> m a
withinNavDiv = RD.elClass "div" "level-item has-text-centered" . RD.el "div"

withinNavPs :: RD.DomBuilder t m => Text -> m b -> m b
withinNavPs heading title =
  RD.elClass "p" "heading" (RD.text heading) >> RD.elClass "p" "title" title

ex03
  :: ( RD.Reflex t
     , MonadFix m
     , RD.MonadHold t m
     , RD.PostBuild t m
     , RD.DomBuilder t m
     )
  => Inputs t
  -> m (Outputs t)
ex03 Inputs {..} =
  let oeError  = RD.filterLeft eBuyAttempt
      oeBuy    = RD.filterRight eBuyAttempt
      oeVend   = oeBuy <&> psProduct
      oeChange = RD.tag bMoney ieRefund
  in  do
        odVend   <- RD.holdDyn Nothing (Just <$> oeVend)
        odChange <- RD.holdDyn 0 oeChange
        odCart   <- RD.foldDyn updateCount mempty oeVend
        dispError
        pure Outputs { .. }

 where
  updateCount     = M.alter $ Just . maybe 1 (+ 1)
  eBuyAttempt     = RD.tag ibSelectedMoney ieBuy <&> checkAttempt
  ibSelectedMoney = (,) <$> bMoney <*> bSelected
  dispError =
    let eErrText = eBuyAttempt <&> either show (const "")
        errAttrs = "style" =: "background-color: red;"
    in  RD.elAttr "div" errAttrs . RD.dynText =<< RD.holdDyn "" eErrText
  bMoney    = RD.current idMoney
  bSelected = RD.current idSelected
  checkAttempt (money, ps@ProductStock {..})
    | psStock <= 0 = Left InsufficientStock
    | money < cost = Left $ InsufficientFunds money cost
    | otherwise    = Right ps
    where cost = pCost psProduct

