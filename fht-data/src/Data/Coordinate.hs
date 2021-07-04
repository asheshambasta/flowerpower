{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
module Data.Coordinate
  ( XY'(..)
  , XY
  , x
  , y
  , pXY
  , distance
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )

-- | Cartesian coordinate
data XY' x y = XY
  { _x :: x -- ^ X coordinate
  , _y :: y -- ^ Y coordinate
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

type XY = XY' Double Double

makeLenses ''XY'
makeAdaptorAndInstance' ''XY'

-- | Simplistic pythagorean distance between 2 points.
--
-- Some explicit type annotations to shut GHC up. 
distance
  :: forall num . (Num num, Floating num) => XY' num num -> XY' num num -> num
distance (XY x0 y0) (XY x1 y1) =
  sqrt $ (sqr $ subtract @num x0 x1) + (sqr $ subtract @num y0 y1)
 where
  sqr :: num -> num
  sqr x' = x' * x'
