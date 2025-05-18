module LunarMagic.Schema.Request where

import LunarMagic.Schema.ContentType (ContentType)

data KRequestBodySpec

foreign import data RequestBodySpec 
  :: Symbol 
  -- ^ description
  -> ContentType 
  -- ^ The type of request body content
  -> KRequestBodySpec

