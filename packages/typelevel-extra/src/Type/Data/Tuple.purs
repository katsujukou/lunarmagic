module Type.Data.Tuple where

-- | The tuple kind which accepts 2 possibly distinct kinds
--   For instance, the type level tuple `Tuple 42 True` has a kind `KTuple Int Boolean`.
data KTuple :: forall k1 k2. k1 -> k2 -> Type
data KTuple k1 k2

infixr 6 type KTuple as *

-- | The type-level tuple, which accepts 2 type-level term and pairs them.
foreign import data Tuple :: forall k1 k2. k1 -> k2 -> KTuple k1 k2

infixr 6 type Tuple as /\

class Fst :: forall k1 k2. KTuple k1 k2 -> k1 -> Constraint
class Fst ab a | ab -> a

instance fstTuple :: Fst (Tuple a b) a

class Snd :: forall k1 k2. KTuple k1 k2 -> k2 -> Constraint
class Snd ab b | ab -> b

instance sndTuple :: Snd (Tuple a b) b