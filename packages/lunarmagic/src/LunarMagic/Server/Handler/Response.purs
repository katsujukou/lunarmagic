-- @inline export stringToResponse.toResponse arity=1
-- @inline export arraybufferToBody.toResponse arity=1
-- @inline export readableStreamToBody.toResponse arity=1
-- @inline export blobToBody.toResponse arity=1
-- @inline export arrayViewToBody.toResponse arity=1
module LunarMagic.Server.Handler.Response
  ( class ToBody
  , toBody
  )
  where

import Data.ArrayBuffer.Types (ArrayBuffer, ArrayView, Uint8Array)
import LunarMagic.Web.Request.Body (Body)
import LunarMagic.Web.Request.Body as Body
import Web.File.Blob (Blob)
import Web.Streams.ReadableStream (ReadableStream)

class ToBody a where
  toBody :: a -> Body

instance stringToBody :: ToBody String where
  toBody = Body.string

instance arraybufferToBody :: ToBody ArrayBuffer where
  toBody = Body.arrayBuffer

instance readableStreamToBody :: ToBody (ReadableStream Uint8Array) where
  toBody = Body.readableStream

instance blobToBody :: ToBody Blob where
  toBody = Body.blob

instance arrayViewToBody :: ToBody (ArrayView a) where
  toBody = Body.arrayView