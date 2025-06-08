module Type.Extra.Row where

import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Type.Data.Maybe as TMaybe

class GetAt :: forall k. Row k -> Symbol -> TMaybe.Maybe k -> Constraint
class GetAt ts at it | ts at -> it

instance
  ( RowToList ts tsL
  , GetAtRowList tsL at it
  ) =>
  GetAt ts at it

class GetAtRowList :: forall k. RowList k -> Symbol -> TMaybe.Maybe k -> Constraint
class GetAtRowList tsL at it | tsL at -> it

instance GetAtRowList RL.Nil at TMaybe.Nothing

instance GetAtRowList (RL.Cons at it tl) at (TMaybe.Just it)
else instance
  ( GetAtRowList tl at it
  ) =>
  GetAtRowList (RL.Cons fld hd tl) at it

class Subset :: Row Type -> Row Type -> Constraint
class Subset p w

class SubsetRowList :: RowList Type -> Row Type -> Constraint
class SubsetRowList pl w

instance SubsetRowList RL.Nil w

instance
  ( SubsetRowList tl w
  , Row.Cons hd a rest w
  ) =>
  SubsetRowList (RL.Cons hd a tl) w