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
main = run 3003 $ liftIO $ mainWidgetWithBulma $ do
  rec _navTotalPlants <- RD.holdDyn 0 RD.never
      let navSettings = Nav.NavSettings { _navInitial = def, .. }
      dNav <- Nav.top navSettings
  RD.elClass "div" "bar" $ RD.text "hi"
  pure ()
