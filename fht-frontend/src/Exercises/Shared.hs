{-# LANGUAGE RankNTypes #-}
module Exercises.Shared
  ( Product(..)
  , Money(..)
  , Stock(..)
  , ProductStock(..)
  , dispMoney
  , dispProduct
  , dispProductStock
  , showMoney
  , showProduct
  , carrot
  , celery
  , cucumber
  )
where

import           Lib.Reflex.Elements            ( emptyEl )
import           Data.Default.Class             ( Default(..) )
import           Control.Monad.Fix              ( MonadFix )
import           Lib.Reflex.Clicks              ( clickEvent )
import           Reflex.Dom
import           Protolude               hiding ( Product )

-- * Types

newtype Stock = Stock Int deriving (Eq, Show, Num, Ord) via Int

instance Default Stock where
  def = 5

newtype Money = Money Int
              deriving (Eq, Show, Num, Ord) via Int

data Product = Product
  { pName :: Text
  , pCost :: Money
  , pImg  :: Text
  }
  deriving (Eq, Show, Ord)

instance Default Product where
  def = carrot

instance Default ProductStock where
  def = ProductStock def def

data ProductStock = ProductStock
  { psProduct :: Product
  , psStock   :: Stock
  }
  deriving (Eq, Show)

carrot :: Product
carrot = Product
  "Carrot"
  1
  "https://i5.walmartimages.ca/images/Enlarge/686/686/6000198686686.jpg"

celery :: Product
celery = Product
  "Celery"
  2
  "https://i5.walmartimages.ca/images/Enlarge/094/529/6000200094529.jpg"

cucumber :: Product
cucumber = Product
  "Cucumber"
  3
  "https://cdn.mos.cms.futurecdn.net/EBEXFvqez44hySrWqNs3CZ.jpg"

dispMoney :: (PostBuild t m, DomBuilder t m) => Dynamic t Money -> m ()
dispMoney dMoney = elDynClass "span" dBadgeClass
  $ dynText (showMoney <$> dMoney)
 where
  dBadgeClass = dMoney
    <&> \m -> "tag is-large " <> if m > 0 then "is-success" else "is-warning"

dispProduct
  :: (DomBuilder t m, PostBuild t m)
  => Dynamic t Product
  -> Product
  -> m (Event t ())
dispProduct = dispProduct' Nothing

dispProduct'
  :: (DomBuilder t m, PostBuild t m)
  => Maybe (Dynamic t Stock)
  -> Dynamic t Product -- ^ A dynamic indicating the selected product.
  -> Product
  -> m (Event t ())
dispProduct' mStock dSelected prod@Product {..} = clickEvent $ fst <$> prodTile
 where
  prodTile =
    elClass' "div" "tile is-parent"
      . elDynAttr "article" prodAttrs -- "tile is-child notification is-info"
      $ do
          elClass "p" "title" $ text pName
          subtitle $ text (showMoney pCost)
          when (isJust mStock) stock'
          elClass "figure" "image is-4by3" $ emptyEl "img" imgAttrs
  imgAttrs  = "src" =: pImg
  stock'    = subtitle $ maybe (text "") (dynText . fmap showStock) mStock
  subtitle  = elClass "p" "subtitle"
  prodAttrs = dSelected <&> \p -> if p == prod
    then prodClass <> ("style" =: "background-color: cadetblue;")
    else prodClass
  prodClass = "class" =: "tile is-child notification is-info"

showStock :: Stock -> Text
showStock s | s > 0     = show s <> " in stock"
            | otherwise = "Sold out"

showMoney :: Money -> Text
showMoney (Money m) = "$" <> show m

showProduct :: Product -> Text
showProduct Product {..} = pName <> " @ " <> showMoney pCost

{- |
Display a product and its stock.
-}
dispProductStock
  :: (DomBuilder t m, PostBuild t m, MonadFix m)
  => Product
  -> Dynamic t Product
  -> Dynamic t Stock
  -> m (Event t Product)
dispProductStock prod dSelected dStock =
  dispProduct' (Just dStock) dSelected prod <&> ($> prod)

