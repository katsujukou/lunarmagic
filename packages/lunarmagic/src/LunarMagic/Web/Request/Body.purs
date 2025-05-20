module LunarMagic.Web.Request.Body where

import Prelude

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8Array)
import Unsafe.Coerce (unsafeCoerce)
import Web.File.Blob (Blob)
import Web.Streams.ReadableStream (ReadableStream)
import Web.XHR.FormData (FormData)

foreign import data Body :: Type 

string :: String -> Body 
string = unsafeCoerce

arrayBuffer :: ArrayBuffer -> Body 
arrayBuffer = unsafeCoerce

arrayView :: forall a. ArrayView a -> Body 
arrayView = unsafeCoerce

readableStream :: ReadableStream Uint8Array -> Body 
readableStream = unsafeCoerce

blob :: Blob -> Body 
blob = unsafeCoerce

formData :: FormData -> Body 
formData = unsafeCoerce
