{-# LANGUAGE RankNTypes #-}
module Frontend.Shared.Widgets
  ( mainWidgetWithBootstrap
  , mainWidgetWithBulma
  , responsive
  , fontAwesome
  )
where

import           Protolude
import           Reflex.Dom                     ( (=:) )
import           Lib.Reflex.Elements            ( emptyEl )
import qualified Reflex.Dom                    as Dom

-- * Common

utf8Charset :: forall x . Dom.Widget x ()
utf8Charset = emptyEl "meta" $ "charset" =: "utf-8"

-- | Adds fontawesome.
-- <script defer src="https://use.fontawesome.com/releases/v5.14.0/js/all.js"></script>
fontAwesome :: forall x . Dom.Widget x ()
fontAwesome = emptyEl "script" attrs
 where
  attrs =
    ("defer" =: "")
      <> ("src" =: "https://use.fontawesome.com/releases/v5.14.0/js/all.js")

-- | Make responsive
-- <meta name="viewport" content="width=device-width, initial-scale=1">
responsive :: forall x . Dom.Widget x ()
responsive = emptyEl "meta" attrs
 where
  attrs =
    ("name" =: "viewport")
      <> ("content" =: "width=device-width, initial-scale=1")

-- * Bulma

-- | Create a widget with bulma stuff. 
-- <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css">
mainWidgetWithBulma :: (forall x . Dom.Widget x ()) -> IO ()
mainWidgetWithBulma = Dom.mainWidgetWithHead $ do
  utf8Charset
  responsive
  fontAwesome
  emptyEl "link" bulmaAttrs
 where
  bulmaAttrs =
    ("rel" =: "stylesheet")
      <> ("href" =: "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"
         )

-- * Bootstrap

-- | Create a widget with all the bootstrap stuff. 
mainWidgetWithBootstrap :: (forall x . Dom.Widget x ()) -> IO ()
mainWidgetWithBootstrap = Dom.mainWidgetWithHead $ do
  utf8Charset
  responsive
  emptyEl "link"   cssAttrs
  emptyEl "script" jqueryAttrs
  emptyEl "script" bootstrapJsAttrs
  fontAwesome
 where
  cssAttrs =
    ("rel" =: "stylesheet")
      <> ("href"
         =: "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/css/bootstrap.min.css"
         )
      <> ("integrity"
         =: "sha384-TX8t27EcRE3e/ihU7zmQxVncDAy5uIKz4rEkgIXeMed4M0jlfIDPvg6uqKI2xXr2"
         )
      <> ("crossorigin" =: "anonymous")
  jqueryAttrs =
    ("src" =: "https://code.jquery.com/jquery-3.5.1.slim.min.js")
      <> ("integrity"
         =: "sha384-DfXdz2htPH0lsSSs5nCTpuj/zy4C+OGpamoFVy38MVBnE+IbbVYUew+OrCXaRkfj"
         )
      <> ("crossorigin" =: "anonymous")
  bootstrapJsAttrs =
    ("src"
      =: "https://cdn.jsdelivr.net/npm/bootstrap@4.5.3/dist/js/bootstrap.bundle.min.js"
      )
      <> ("integrity"
         =: "sha384-ho+j7jyWK8fNQe+A12Hb8AhRq26LrZ/JpcUGGOn+Y7RsweNrtN/tE3MoK7ZeZDyx"
         )
      <> ("crossorigin" =: "anonymous")

