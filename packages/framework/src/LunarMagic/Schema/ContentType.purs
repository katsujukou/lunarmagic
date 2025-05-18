module LunarMagic.Schema.ContentType where

import Type.Data.Maybe (Just, Maybe, Nothing)

-- | The type of the content type of the request/response body.
data ContentType

foreign import data ApplicationJson :: Type -> ContentType 

class FromContentType (a :: ContentType) (s :: Symbol)

instance FromContentType (ApplicationJson t) "application/json"

class ToContentType :: Symbol -> Maybe ContentType -> Constraint
class ToContentType s c | s -> c 

instance ToContentType "application/json" (Just (ApplicationJson t))
else instance ToContentType _1 Nothing