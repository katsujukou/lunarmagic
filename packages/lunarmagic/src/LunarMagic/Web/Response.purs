module LunarMagic.Web.Response
  ( Response
  , clone
  , defaultResponseInit
  , empty
  , new
  )
  where


import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import LunarMagic.Web.Headers (Headers)
import LunarMagic.Web.Headers as Headers
import LunarMagic.Web.Request.Body (Body)

-- | The type of the response body
-- @see https://developer.mozilla.org/docs/Web/API/Response
foreign import data Response :: Type

type ResponseInit =
  { headers :: Headers
  , statusText :: String
  , status :: Int
  }

foreign import new_ :: EffectFn2 (Nullable Body) ResponseInit Response

defaultResponseInit :: ResponseInit
defaultResponseInit =
  { headers: Headers.empty
  , statusText: ""
  , status: 200
  }

new :: Maybe Body -> ResponseInit -> Effect Response
new body = runEffectFn2 new_ (toNullable body)

empty :: Effect Response 
empty = new Nothing defaultResponseInit

foreign import clone_ :: EffectFn1 Response Response

clone :: Response -> Effect Response
clone  = runEffectFn1 clone_ 