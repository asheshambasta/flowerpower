{-# LANGUAGE RecursiveDo #-}
module Main
  ( main
  )
where

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
  navBar (RD.constDyn . length $ plants)
  dispPlants (headMay plants) (RD.constDyn plants)
  pure ()
  where plants = [plant0, plant1]

navBar
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m)
  => RD.Dynamic t Int
  -> m (RD.Dynamic t Nav.Navigation)
navBar _navTotalPlants =
  Tags.navClass "navbar-menu" . Tags.divClass "container" $ do
  -- _navTotalPlants <- RD.holdDyn 0 RD.never
    let navSettings = Nav.NavSettings { _navInitial = def, .. }
    Nav.top navSettings

dispPlants'
  :: (RD.DomBuilder t m, RD.PostBuild t m, RD.MonadHold t m, MonadFix m) => m ()
dispPlants' = sectionContainer . tileSection $ do
  rec dSelectedPlant <- fmap Just <$> RD.holdDyn plant0 ePlant
      eP0            <- plantCard dSelectedPlant plant0
      eP1            <- plantCard dSelectedPlant plant1
      let ePlant = RD.leftmost [eP0, eP1]
  pure ()

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
