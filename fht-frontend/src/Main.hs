{-# LANGUAGE RecursiveDo #-}
module Main
  ( main
  )
where

import qualified Data.Map                      as M
import           Frontend.Shared.Widgets.Bulma  ( sectionContainer
                                                , tileSection
                                                )
import           Frontend.Garden.Plant
import           Control.Monad.Fix              ( MonadFix )
import qualified Frontend.Nav                  as Nav
import           Data.Default.Class             ( def )
import qualified "reflex-dom-helpers" Reflex.Tags
                                               as Tags
import           Control.Lens
import qualified Reflex.Dom                    as RD
import           Frontend.Shared.Widgets        ( mainWidgetWithBulma )
import           Protolude

main :: IO ()
main = mainWidgetWithBulma $ do
  rec dNav      <- navBar (RD.constDyn . length $ plants)
      eAddLogs  <- sectionContainer . plantMaintenance dSelected $ Summary
      eSelected <- dispPlants initSelected
                              (Nav.dNavFilterPlants dNav)
                              (RD.constDyn plants)
                                        -- currently selected  plant.
      dSelected <- RD.holdDyn initSelected (Just <$> eSelected)
  pure ()
 where
  plants       = [fpd0, fpd1]
  initSelected = Nothing -- headMay plants

navBar
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t Int
  -> m (RD.Dynamic t Nav.Navigation)
navBar _navTotalPlants =
  Tags.sectionClass "section"
    . Tags.navClass "navbar-menu"
    . Tags.divClass "container"
    $ do
  -- _navTotalPlants <- RD.holdDyn 0 RD.never
        let navSettings = Nav.NavSettings { _navInitial = def, .. }
        Nav.top navSettings

plant0 :: Plant
plant0 = Plant { _pId               = 0
               , _pName             = "Japanese Maple"
               , _pDesc = Just "Maple from Japan, bought from 'Art of flower'"
               , _pImage            = Nothing
               , _pTimePlanted      = Nothing
               , _pMaintenanceTypes = mempty
               , _pMaintenanceFreqs = mempty
               }

plant1 :: Plant
plant1 = Plant { _pId               = 1
               , _pName             = "Black lace"
               , _pDesc             = Just "Black lace, flowers in winter."
               , _pImage            = Nothing
               , _pTimePlanted      = Nothing
               , _pMaintenanceTypes = mempty
               , _pMaintenanceFreqs = mempty
               }

fpd0 = FullPlantData plant0 mempty
-- fpd1 = FullPlantData plant1 mempty
fpd1 = FullPlantData
  plant1
  (M.fromList
    [(Pruning, DueBy Year (Just 10)), (Fertilizing, DueBy Month Nothing)]
  )
-- this causes the Jsaddle-warp server tp hang: the page never loads and the cabal process needs to be killed.
-- fpd1 = FullPlantData plant1 (M.singleton Pruning (DueBy Year Nothing))
