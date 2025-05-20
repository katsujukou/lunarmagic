module LunarMagic.Server.Handler.Spec where

import Type.Data.Maybe (class FromMaybe)
import Type.Extra.Row (class GetAt)

data HandlerSpec 

foreign import data ApiHandlerSpec :: Row Type -> HandlerSpec 

class SpecResponses :: HandlerSpec -> Type -> Constraint
class SpecResponses spec resps | spec -> resps 

instance 
  ( GetAt specR "responses" mbResps 
  , FromMaybe {} mbResps resps 
  ) => SpecResponses (ApiHandlerSpec specR) resps 

