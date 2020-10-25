{-# LANGUAGE RecursiveDo #-}
module Main
  ( main
  )
where

import           Language.Javascript.JSaddle.Warp
                                                ( run )
import qualified Frontend.Nav                  as Nav
import           Data.Default.Class             ( def )
import           Control.Lens
import qualified Reflex.Dom                    as RD
import           Frontend.Shared.Widgets        ( mainWidgetWithBulma )
import           Protolude

main :: IO ()
main = mainWidgetWithBulma $ do
  rec _navTotalPlants <- RD.holdDyn 0 RD.never
      _navDyn         <- RD.holdDyn def RD.never -- navEvt
      let navSettings = Nav.NavSettings { .. }
      navEvt <- Nav.top navSettings
  RD.elClass "div" "bar" $ RD.text "hi"
  pure ()
