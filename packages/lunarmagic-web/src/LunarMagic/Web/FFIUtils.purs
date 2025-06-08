-- @inline export unsafeAsRecord always
-- @inline export unsafeGetProperty arity=1
-- @inline unsafeGetMethodProperty arity=2
module LunarMagic.Web.FFIUtils where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Symbol (class IsSymbol)
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

unsafeAsRecord :: forall f r. f r -> Record r
unsafeAsRecord = unsafeCoerce

foreign import unsafeGetMethodPropertyFn :: forall a b. Fn2 a String b

unsafeGetMethodProperty :: forall a b. a -> String -> b
unsafeGetMethodProperty = runFn2 unsafeGetMethodPropertyFn

infix 5 unsafeGetMethodProperty as #->

unsafeGetProperty
  :: forall @prop r t f _1
   . IsSymbol prop
  => Row.Cons prop t _1 r
  => f r
  -> t
unsafeGetProperty = unsafeAsRecord >>> Record.get (Proxy :: _ prop)