-- @inline export toHeadersFieldsNil(..).toHeadersFields arity=1 
-- @inline export toHeadersFieldsCons(..).toHeadersFields arity=1 
-- @inline export toHeaders arity=1 
module LunarMagic.Server.Headers where

import Prelude

import Data.Array as Array
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Newtype (over, wrap)
import Data.String.CaseInsensitive (CaseInsensitiveString(..))
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Tuple (Tuple)
import Foreign.Object as Object
import LunarMagic.Web.Headers as Web
import LunarMagic.Web.Headers.Init as Init
import Prim.Row as Row
import Prim.RowList (class RowToList, RowList)
import Prim.RowList as RL
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

class HeaderValue a where
  toHeaderValue :: a -> Array String

instance headerValueString :: HeaderValue String where
  toHeaderValue = Array.singleton

instance headerValueArrayString :: HeaderValue (Array String) where
  toHeaderValue = identity

newtype Header = Header (Tuple CaseInsensitiveString (Array String))

newtype Headers = Headers (SemigroupMap CaseInsensitiveString (Array String))

derive newtype instance semigroupHeaders :: Semigroup Headers 

lookup :: String -> Headers -> Maybe (Array String)
lookup k (Headers (SemigroupMap headers)) = Map.lookup (wrap k) headers 

toHeaders :: forall r rl. RowToList r rl => ToHeadersFields r rl => Record r -> Headers 
toHeaders = toHeadersFields @_ @rl >>> Headers

toWeb :: Headers -> Web.Headers 
toWeb (Headers (SemigroupMap headers)) = Web.new $ Init.fromObject $
  foldMapWithIndex 
    (\(CaseInsensitiveString k) v -> Object.singleton k v)
    headers

empty :: Headers 
empty = Headers (SemigroupMap Map.empty)

t :: Headers 
t = toHeaders 
  { "Content-Type": [ "application/json", "application/xml"] 
  , "Accept": "text/plain"
  }

class ToHeadersFields :: Row Type -> RowList Type -> Constraint 
class ToHeadersFields r rl | rl -> r where
  toHeadersFields :: Record r -> SemigroupMap CaseInsensitiveString (Array String) 

instance toHeadersFieldsNil :: ToHeadersFields () RL.Nil where
  toHeadersFields _ = wrap Map.empty

instance toHeadersFieldsCons ::
  ( HeaderValue a
  , ToHeadersFields rest tl
  , IsSymbol fld
  , Row.Cons fld a rest rows 
  ) =>  ToHeadersFields rows (RL.Cons fld a tl) 
  where
    toHeadersFields r = 
      let 
        k = reflectSymbol (Proxy :: _ fld)
        a = Record.get (Proxy :: _ fld) r
        r' = (unsafeCoerce :: Record rows -> Record rest) r
      in 
        over SemigroupMap (Map.insert (wrap k) (toHeaderValue a)) (toHeadersFields @_ @tl r')