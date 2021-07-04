{-# LANGUAGE
    TemplateHaskell
  , DeriveAnyClass
#-}
module Data.Coordinate
  ( XY'(..)
  , XY
  , x
  , y
  , pXY
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )

-- | Cartesian coordinates 
data XY' x y = XY
  { _x :: x
  , _y :: y
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type XY = XY' Double Double

makeLenses ''XY'
makeAdaptorAndInstance' ''XY'
