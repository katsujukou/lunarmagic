-- @inline Data.Foldable.foldableArray.foldMap arity=1
-- @inline export fromFoldable arity=1
-- @inline export empty always
-- @inline export fromArray arity=1
module LunarMagic.Web.Headers where

import Prelude

import Data.Function.Uncurried (Fn1, runFn1)
import LunarMagic.Web.Headers.Init (HeadersInit, fromRecord)
import Unsafe.Coerce (unsafeCoerce)

toHeadersInit :: Headers -> HeadersInit 
toHeadersInit = unsafeCoerce

foreign import data Headers :: Type 

foreign import new_ :: Fn1 
  HeadersInit
  Headers

new :: HeadersInit -> Headers 
new = runFn1 new_ 

empty :: Headers 
empty = new $ fromRecord {}
