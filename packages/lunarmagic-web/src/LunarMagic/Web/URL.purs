module LunarMagic.Web.URL where

import Prelude

import Data.Function.Uncurried (Fn2, Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import LunarMagic.Web.FFIUtils (unsafeGetProperty, (#->))
import Type.Row (type (+))

foreign import data RawURL :: Type

foreign import newFn :: EffectFn1 String RawURL

new :: String -> Effect (URL All)
new = runEffectFn1 newFn >>> map URL

newtype URL (r :: Row Type) = URL RawURL

instance showURL :: Show (URL r) where
  show _ = "(URL (...))"

foreign import data URLSearchParams :: Type

type STATIC r =
  ( hash :: String
  , host :: String
  , hostname :: String
  , href :: String
  , origin :: String
  , password :: String
  , pathname :: String
  , port :: String
  , protocol :: String
  , search :: String
  , searchParams :: URLSearchParams
  , username :: String
  | r
  )

type EFFECT r =
  ( toString :: Unit -> String
  , toJSON :: Unit -> String
  | r
  )

type Static = STATIC ()

type All = (STATIC + EFFECT + ())

hash :: forall r. URL (STATIC + r) -> Effect String
hash = unsafeGetProperty @"hash" >>> pure

host :: forall r. URL (STATIC + r) -> Effect String
host = unsafeGetProperty @"host" >>> pure

hostname :: forall r. URL (STATIC + r) -> Effect String
hostname = unsafeGetProperty @"hostname" >>> pure

href :: forall r. URL (STATIC + r) -> Effect String
href = unsafeGetProperty @"href" >>> pure

origin :: forall r. URL (STATIC + r) -> Effect String
origin = unsafeGetProperty @"origin" >>> pure

password :: forall r. URL (STATIC + r) -> Effect String
password = unsafeGetProperty @"password" >>> pure

pathname :: forall r. URL (STATIC + r) -> Effect String
pathname = unsafeGetProperty @"pathname" >>> pure

port :: forall r. URL (STATIC + r) -> Effect String
port = unsafeGetProperty @"port" >>> pure

protocol :: forall r. URL (STATIC + r) -> Effect String
protocol = unsafeGetProperty @"protocol" >>> pure

search :: forall r. URL (STATIC + r) -> Effect String
search = unsafeGetProperty @"search" >>> pure

searchParams :: forall r. URL (STATIC + r) -> Effect URLSearchParams
searchParams = unsafeGetProperty @"searchParams" >>> pure

username :: forall r. URL (STATIC + r) -> Effect String
username = unsafeGetProperty @"username" >>> pure

toString :: forall r. URL (EFFECT + r) -> Effect String
toString url = pure (_toString url)
  where
  _toString = url #-> "toString"

toJSON :: forall r. URL (EFFECT + r) -> Effect String
toJSON url = pure (_toJSON url)
  where
  _toJSON = url #-> "toJSON"

foreign import canParseFn :: String -> Boolean

foreign import canParseWithBaseFn :: Fn2 String String Boolean

foreign import parseFn :: forall r. Fn3 (forall a. Maybe a) (forall a. a -> Maybe a) String (Maybe (URL r))

foreign import parseWithBaseFn :: forall r. Fn4 (forall a. Maybe a) (forall a. a -> Maybe a) String String (Maybe (URL r))

parse :: forall r. String -> Maybe (URL r)
parse = runFn3 parseFn Nothing Just

parseWithBase :: forall r. String -> String -> Maybe (URL r)
parseWithBase = runFn4 parseWithBaseFn Nothing Just

