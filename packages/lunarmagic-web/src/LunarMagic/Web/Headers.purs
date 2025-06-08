-- @inline Data.Foldable.foldableArray.foldMap arity=1
-- @inline export fromFoldable arity=1
-- @inline export empty always
-- @inline export fromArray arity=1
module LunarMagic.Web.Headers where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import Data.Maybe (Maybe)
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, EffectFn2, EffectFn3, runEffectFn1, runEffectFn2, runEffectFn3)
import Foreign.Object (Object)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

foreign import data HeadersInit :: Type

unsafeFromArray :: Array (Array String) -> HeadersInit
unsafeFromArray = unsafeCoerce

fromObject :: Object (Array String) -> HeadersInit
fromObject = unsafeCoerce

fromRecord :: forall r. Homogeneous r (Array String) => Record r -> HeadersInit
fromRecord = unsafeCoerce

fromHeaders :: Headers -> HeadersInit
fromHeaders = unsafeCoerce

-- | Mutable headers object
-- One can modify the entire headers with effectful setting, appending and deleting operation.
foreign import data Headers :: Type

foreign import newFn
  :: Fn1
       HeadersInit
       Headers

new :: HeadersInit -> Headers
new = runFn1 newFn

empty :: Headers
empty = new $ fromRecord {}

foreign import appendFn :: EffectFn3 String String Headers Unit

append :: String -> String -> Headers -> Effect Unit
append = runEffectFn3 appendFn

foreign import deleteFn :: EffectFn2 String Headers Unit

delete :: String -> Headers -> Effect Unit
delete = runEffectFn2 deleteFn

foreign import getFn :: EffectFn2 String Headers (Nullable String)

get :: String -> Headers -> Effect (Maybe String)
get k h = runEffectFn2 getFn k h <#> toMaybe

foreign import getSetCookieFn :: EffectFn1 Headers (Array String)

getSetCookie :: Headers -> Effect (Array String)
getSetCookie = runEffectFn1 getSetCookieFn

foreign import hasFn :: EffectFn2 String Headers Boolean

has :: String -> Headers -> Effect Boolean
has = runEffectFn2 hasFn

foreign import setFn :: EffectFn3 String String Headers Unit

set :: String -> String -> Headers -> Effect Unit
set = runEffectFn3 setFn