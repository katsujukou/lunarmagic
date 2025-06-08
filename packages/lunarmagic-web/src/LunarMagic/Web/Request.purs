-- @inline export unsafeGetProperty arity=1
module LunarMagic.Web.Request
  ( AbortSignal
  , All
  , BODY
  , Body
  , EFFECT
  , Request
  , STATIC
  , Static
  , arrayBuffer
  , blob
  , body
  , bodyUsed
  , bytes
  , cache
  , clone
  , credentials
  , destination
  , formData
  , headers
  , integrity
  , isHistoryNavigation
  , json
  , keepalive
  , method
  , mode
  , rediret
  , referrer
  , referrerPolicy
  , text
  , url
  ) where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, Uint8Array)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (Foreign)
import LunarMagic.Web.FFIUtils (unsafeGetProperty, (#->))
import LunarMagic.Web.Headers (Headers)
import Promise (Promise)
import Promise.Aff (toAffE)
import Safe.Coerce (coerce)
import Type.Row (type (+))
import Web.File.Blob (Blob)
import Web.Streams.ReadableStream (ReadableStream)
import Web.XHR.FormData (FormData)

foreign import data RawRequest :: Type

foreign import cloneFn :: EffectFn1 RawRequest RawRequest

clone :: forall r. Request r -> Effect (Request All)
clone = coerce >>> runEffectFn1 cloneFn >>> map Request

newtype Request (r :: Row Type) = Request RawRequest

type STATIC r =
  ( bodyUsed :: Boolean
  , cache :: String
  , credentials :: String
  , destination :: String
  , headers :: Headers
  , integrity :: String
  , isHistoryNavigation :: Boolean
  , keepalive :: Boolean
  , method :: String
  , mode :: String
  , rediret :: String
  , referrer :: String
  , referrerPolicy :: String
  , url :: String
  | r
  )

type BODY r =
  ( body :: ReadableStream Uint8Array
  , blob :: Effect (Promise Blob)
  , bytes :: Effect (Promise Uint8Array)
  , formData :: Effect (Promise FormData)
  , json :: Effect (Promise Foreign)
  , text :: Unit -> Promise String
  | r
  )

foreign import data AbortSignal :: Type

type EFFECT r =
  ( signal :: AbortSignal
  | r
  )

type All = (STATIC + BODY + EFFECT ())

type Body = (STATIC + BODY ())

type Static = (STATIC ())

bodyUsed :: forall r. Request (STATIC + r) -> Effect Boolean
bodyUsed = unsafeGetProperty @"bodyUsed" >>> pure

cache :: forall r. Request (STATIC + r) -> Effect String
cache = unsafeGetProperty @"cache" >>> pure

credentials :: forall r. Request (STATIC + r) -> Effect String
credentials = unsafeGetProperty @"credentials" >>> pure

destination :: forall r. Request (STATIC + r) -> Effect String
destination = unsafeGetProperty @"destination" >>> pure

headers :: forall r. Request (STATIC + r) -> Effect Headers
headers = unsafeGetProperty @"headers" >>> pure

integrity :: forall r. Request (STATIC + r) -> Effect String
integrity = unsafeGetProperty @"integrity" >>> pure

isHistoryNavigation :: forall r. Request (STATIC + r) -> Effect Boolean
isHistoryNavigation = unsafeGetProperty @"isHistoryNavigation" >>> pure

keepalive :: forall r. Request (STATIC + r) -> Effect Boolean
keepalive = unsafeGetProperty @"keepalive" >>> pure

method :: forall r. Request (STATIC + r) -> Effect String
method = unsafeGetProperty @"method" >>> pure

mode :: forall r. Request (STATIC + r) -> Effect String
mode = unsafeGetProperty @"mode" >>> pure

rediret :: forall r. Request (STATIC + r) -> Effect String
rediret = unsafeGetProperty @"rediret" >>> pure

referrer :: forall r. Request (STATIC + r) -> Effect String
referrer = unsafeGetProperty @"referrer" >>> pure

referrerPolicy :: forall r. Request (STATIC + r) -> Effect String
referrerPolicy = unsafeGetProperty @"referrerPolicy" >>> pure

url :: forall r. Request (STATIC + r) -> Effect String
url = unsafeGetProperty @"url" >>> pure

body :: forall r. Request (BODY + r) -> Effect (ReadableStream Uint8Array)
body = unsafeGetProperty @"body" >>> pure

text :: forall r. Request (BODY + r) -> Aff String
text req = toAffE $ pure $ _text (coerce req)
  where
  _text :: RawRequest -> Promise String
  _text = req #-> "text"

json :: forall r. Request (BODY + r) -> Aff Foreign
json req = toAffE $ pure $ _json (coerce req)
  where
  _json :: RawRequest -> Promise Foreign
  _json = req #-> "json"

blob :: forall r. Request (BODY + r) -> Aff Blob
blob req = toAffE $ pure $ _blob (coerce req)
  where
  _blob :: RawRequest -> Promise Blob
  _blob = req #-> "blob"

bytes :: forall r. Request (BODY + r) -> Aff (Uint8Array)
bytes req = toAffE $ pure $ _bytes (coerce req)
  where
  _bytes :: RawRequest -> Promise Uint8Array
  _bytes = req #-> "bytes"

arrayBuffer :: forall r. Request (BODY + r) -> Aff ArrayBuffer
arrayBuffer req = toAffE $ pure $ _arrayBuffer (coerce req)
  where
  _arrayBuffer :: RawRequest -> Promise ArrayBuffer
  _arrayBuffer = req #-> "arrayBuffer"

formData :: forall r. Request (BODY + r) -> Aff FormData
formData req = toAffE $ pure $ _formData (coerce req)
  where
  _formData :: RawRequest -> Promise FormData
  _formData = req #-> "formData"