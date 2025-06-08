module Type.Data.List where

import Prim.Boolean (False, True)
import Prim.Int as Int
import Prim.TypeError (Text)
import Type.Data.Maybe as TMaybe

-- | Kind fot type-level lists whose elements are type-level terms of kind `k`.
data List :: forall k. k -> Type
data List k

-- | Type constructor for the empty type-level list. 
foreign import data Nil :: forall k. List k

-- | Type constructor for non-empty type-level lists.
foreign import data Cons :: forall k. k -> List k -> List k

infixr 5 type Cons as :

-- | Partially extracts the 0-th element from given type-level list.
--  When the input list is empty, a type error will be thrown.  
class Head :: forall k. List k -> k -> Constraint
class Head xs x | xs -> x

instance
  ( HeadMaybe xs mbx
  , TMaybe.FromJustOrFail
      (Text "Failed pattern match at type-level function Type.Data.List.Head")
      mbx
      x
  ) =>
  Head xs x

-- | Total version of `Head` with return type wrapped with type-level `Maybe`. 
class HeadMaybe :: forall k. List k -> TMaybe.Maybe k -> Constraint
class HeadMaybe xs mx | xs -> mx

instance headMaybeNil :: HeadMaybe Nil TMaybe.Nothing
instance headMaybeCons :: HeadMaybe (Cons x xs) (TMaybe.Just x)

-- | Extracts the tail of a type-level list. If the input list is empty, a type error will be thrown.
class Tail :: forall k. List k -> List k -> Constraint
class Tail xs ys | xs -> ys

instance
  ( TailMaybe xs mbys
  , TMaybe.FromJustOrFail
      (Text "Failed pattern match at type-level function Type.Data.List.Tail")
      mbys
      ys
  ) =>
  Tail xs ys

-- | Total version of `Tail` with return type wrapped with type-level `Maybe`.
class TailMaybe :: forall k. List k -> TMaybe.Maybe (List k) -> Constraint
class TailMaybe xs ys | xs -> ys

instance tailMaybeNil :: TailMaybe Nil TMaybe.Nothing

instance tailMaybeCons :: TailMaybe (Cons x xs) (TMaybe.Just xs)

-- | Appends two type-level lists to produce a new type-level list.
class Append :: forall k. List k -> List k -> List k -> Constraint
class Append xs ys zs | xs ys -> zs

instance appendNil :: Append Nil ys ys

instance appendCons :: Append xs ys zs => Append (Cons x xs) ys (Cons x zs)

-- | Updates the element at the specified index in a type-level list.
--   If the index is out of bounds, the behavior depends on the instance.
class UpdateAt :: forall k. Int -> k -> List k -> List k -> Constraint
class UpdateAt n a xs ys | n a xs -> ys

instance UpdateAt 0 a (Cons _1 xs) (Cons a xs)
else instance UpdateAt i a Nil Nil
else instance
  ( Int.Add (-1) i j
  , UpdateAt j a xs ys
  ) =>
  UpdateAt i a (Cons x xs) (Cons x ys)

-- | Extracts the element at the specified index in a type-level list.
--   If the index is out of bounds, a type error will be thrown.
class At :: forall k. Int -> List k -> k -> Constraint
class At i xs x | i xs -> x

instance
  ( AtMaybe i xs mbx
  , TMaybe.FromJustOrFail
      (Text "Failed pattern match at type-level function Type.Data.List.At")
      mbx
      x
  ) =>
  At i xs x

-- | Total version of `At` with return type wrapped with type-level `Maybe`.
class AtMaybe :: forall k. Int -> List k -> TMaybe.Maybe k -> Constraint
class AtMaybe i xs mbx | i xs -> mbx

instance AtMaybe 0 Nil TMaybe.Nothing
else instance AtMaybe 0 (Cons x xs) (TMaybe.Just x)
else instance
  ( Int.Add (-1) n pn
  , AtMaybe pn xs mbx
  ) =>
  AtMaybe n (Cons x xs) mbx

-- | Determines whether a type-level list is empty.
--   Produces `True` if the list is empty, otherwise `False`.
class Null :: forall k. List k -> Boolean -> Constraint
class Null xs b | xs -> b

instance Null Nil True
else instance Null _1 False

-- | Computes the length of a type-level list.
class Length :: forall k. List k -> Int -> Constraint
class Length xs n | xs -> n

instance lengthNil :: Length Nil 0
instance lengthCons ::
  ( Length tl n'
  , Int.Add n' 1 n
  ) =>
  Length (Cons hd tl) n

-- | Extracts the first `n` elements from a type-level list.
--   If `n` is greater than the length of the list, the entire list is returned.
class Take :: forall k. Int -> List k -> List k -> Constraint
class Take n xs ys | n xs -> ys

instance Take 0 xs Nil
else instance
  ( Int.Add (-1) n pn
  , Take pn xs ys
  ) =>
  Take n (Cons x xs) (Cons x ys)
else instance Take n Nil Nil

-- | Drops the first `n` elements from a type-level list.
--   If `n` is greater than the length of the list, an empty list is returned.
class Drop :: forall k. Int -> List k -> List k -> Constraint
class Drop n xs ys | n xs -> ys

instance Drop n Nil Nil

else instance Drop 0 xs xs
else instance
  ( Int.Add (-1) n pn
  , Drop pn xs ys
  ) =>
  Drop n (Cons x xs) ys

class Reverse :: forall k. List k -> List k -> Constraint
class Reverse xs ys | xs -> ys, ys -> xs

instance reverseNil :: Reverse Nil Nil
else instance reveresCons ::
  ( Reverse tl lt
  , Append lt (hd : Nil) rev
  ) =>
  Reverse (hd : tl) rev