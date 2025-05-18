-- | This module provides additional utilities for working with the `Maybe` type
-- | at the type level. It includes functionality for handling optional values
-- | and interacting with type-level computations.
module Type.Data.Maybe where

import Prim.Boolean (False, True)
import Prim.TypeError (class Fail, Doc)

-- | The kind of type-level `Maybe`, representing an optional type-level term.
data Maybe :: forall k. k -> Type
data Maybe k

-- | A type-level representation of absense of a value
foreign import data Nothing :: forall k. Maybe k

-- | A type-level representation of a value of kind `k`.
foreign import data Just :: forall k. k -> Maybe k

-- | A type-level function for extracting a type from `Maybe`-kinded type, providing a default
-- | value in case the input is `Nothing`.
class FromMaybe :: forall k. k -> Maybe k -> k -> Constraint
class FromMaybe default maybe o | default maybe -> o

instance FromMaybe def Nothing def
instance FromMaybe _1 (Just k) k

-- | A type-level function for extracting a type from a `Maybe`-kinded type.
--  Unlike `FromMaybe`, this function throws a compile-time error specified in the first argument
--  if the input is `Nothing`.
class FromJustOrFail :: forall k. Doc -> Maybe k -> k -> Constraint
class FromJustOrFail err mbk k | err mbk -> k

instance Fail err => FromJustOrFail err Nothing k
instance FromJustOrFail err (Just k) k

-- | A type class for checking if a `Maybe` type is `Nothing`.
class IsNothing :: forall k. Maybe k -> Boolean -> Constraint
class IsNothing mb b | mb -> b

instance IsNothing Nothing True
instance IsNothing (Just k) False
