-- @inline export eqResponseType.eq arity=2
-- @inline export showRespnoseType.show arity=1
-- @inline export toResponseType arity=1
module LunarMagic.Web.Response.ResponseType where

import Prelude

import Data.Maybe (Maybe(..))

data ResponseType 
  = Basic
  | CORS
  | Default
  | Error
  | Opaque
  | OpaqueRedirect

derive instance eqResponseType :: Eq ResponseType 

instance showResponseType :: Show ResponseType where
  show Basic = "Basic"
  show CORS = "CORS"
  show Default = "Default"
  show Error = "Error"
  show Opaque = "Opaque"
  show OpaqueRedirect = "OpaqueRedirect"

toResponseType :: String -> Maybe ResponseType 
toResponseType = case _ of 
  "Basic" -> Just Basic
  "CORS" -> Just CORS
  "Default" -> Just Default
  "Error" -> Just Error
  "Opaque" -> Just Opaque
  "OpaqueRedirect" -> Just OpaqueRedirect
  _ -> Nothing 
