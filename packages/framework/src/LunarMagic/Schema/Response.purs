module LunarMagic.Schema.Response where

import LunarMagic.Schema.ContentType (ContentType)

data KResponseSpec 

foreign import data ResponseSpec 
  :: Symbol 
  -- ^ description
  -> ContentType 
  -- ^ The type of response body content
  -> KResponseSpec 

