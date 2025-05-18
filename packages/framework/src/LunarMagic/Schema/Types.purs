module LunarMagic.Schema.Types where

import LunarMagic.Schema.Types.Request (KRequestBodySpec)
import LunarMagic.Schema.Types.Response (KResponseSpec)
import Type.Data.Maybe (Maybe)

data KSchema

foreign import data Schema 
  :: KSchemaMetainfo
  -> Row KSchemaEndpoints 
  -> KSchema

data KSchemaMetainfo

foreign import data SchemaMetainfo 
  :: Symbol
  -- ^ The name of API 
  -> Symbol
  -- ^ The version of API
  -> Symbol
  -- ^ The description of API
  -> KSchemaMetainfo

data KSchemaEndpoints

foreign import data SchemaEndpoints 
  :: Row KSchemaEndpoint
  -- ^ The set of endpoints of the API
  -- (each key is the method (get, post, ...) of the endpoint)
  -> KSchemaEndpoints

data KSchemaEndpoint 

foreign import data SchemaEndpoint 
  :: Symbol 
  -- ^ The name of the endpoint 
  -> Maybe Symbol 
  -- ^ The description of the endpoint
  -> KEndpointSpec 
  -- ^ The endpoint specification 
  -- (request parameters, request body types, response body types)
  -> KSchemaEndpoint

data KEndpointSpec 

foreign import data EndpointSpec 
  :: Row Type
  -- ^ the set of path parameter types 
  -- (each key is the name of the parameter which must be specified
  -- in the path string in the form of `:parameter` )
  -> Row Type 
  -- ^ the set of the query parameter types
  -> Maybe KRequestBodySpec
  -- ^ the request body type
  -> Row KResponseSpec
  -- ^ e.g: ( "200" :: ResponseSpec (...) )
  -> KEndpointSpec