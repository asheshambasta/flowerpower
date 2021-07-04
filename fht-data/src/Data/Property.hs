{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveAnyClass #-}
{-|
Module: Data.Property
Description: Propertyerty: a collection of 0 or more gardens: interior or exterior.
-}
module Data.Property
  ( Property'(..)
  , Property
  , pId
  , pType
  , pName
  , pDesc
  , pProperty
  , PropertyId'(..)
  , PropertyId
  , unPropertyId
  , pPropertyId
  , PropertyType(..)
  ) where

import           Control.Lens
import           Data.Aeson
import           Data.Profunctor.Product.TH     ( makeAdaptorAndInstance' )
import           Web.HttpApiData                ( FromHttpApiData
                                                , ToHttpApiData
                                                )

data Property' id ptype name desc = Property
  { _pId   :: id -- ^ ID of the property 
  , _pType :: ptype -- ^ Property type 
  , _pName :: name -- ^ Name 
  , _pDesc :: desc -- ^ Description 
  }
  deriving (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type Property = Property' PropertyId PropertyType Text (Maybe Text)

newtype PropertyId' id = PropertyId { _unPropertyId :: id }
                   deriving (Eq, Show, ToJSON, FromJSON, ToHttpApiData, FromHttpApiData , Ord, Num, Real, Enum, Integral) via id

type PropertyId = PropertyId' Int64

-- | Type of property 
data PropertyType = Home | Office
              deriving (Eq, Show, Generic)
              deriving anyclass (ToJSON, FromJSON)

makeLenses ''Property'
makeAdaptorAndInstance' ''Property'
makeLenses ''PropertyId'
makeAdaptorAndInstance' ''PropertyId'
