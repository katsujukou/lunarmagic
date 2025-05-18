module LunarMagic.Schema
  ( module ReExports
  ) where

import Data.Maybe (Maybe)
import LunarMagic.Schema.Types (EndpointSpec, Schema, SchemaEndpoint, SchemaEndpoints, SchemaMetainfo)
import LunarMagic.Schema.Types.ContentType (ApplicationJson)
import LunarMagic.Schema.Types.Request (RequestBodySpec)
import LunarMagic.Schema.Types.Response (ResponseSpec)
import Type.Data.Maybe (Just, Nothing)

import LunarMagic.Schema.Types (Schema, SchemaMetainfo, SchemaEndpoints, SchemaEndpoint, EndpointSpec) as ReExports
import LunarMagic.Schema.Request (RequestBodySpec) as ReExports 
import LunarMagic.Schema.Response (ResponseSpec) as ReExports 
import LunarMagic.Schema.ContentType (ApplicationJson) as ReExports

type SampleSchema = Schema
  ( SchemaMetainfo
      -- The name of the API
      "The example API"
      -- The description of the API
      "The example of the API schema \
      \encoded in the typelevel machinery in the LunarMagic"
      -- The version of the API
      "v1.0.0"
  )
  -- The set of API endpoints
  ( "/api/todos" ::
      SchemaEndpoints
        ( "get" ::
            SchemaEndpoint
              "list_todos"
              (Just "list todos")
              ( EndpointSpec
                  -- The type of the path parameters 
                  -- (empty in this case)
                  ()
                  -- The type of the query parameters
                  ( sort :: Maybe String
                  , limit :: Maybe Int
                  , page :: Maybe Int
                  )
                  -- The type of the request body
                  -- (nothing in this case)
                  Nothing
                  -- The type of the response body in the form of 
                  -- row of response content (status code in the key)
                  ( "200" ::
                      ResponseSpec
                        "The success response"
                        ( ApplicationJson
                            ( Array
                                { id :: Int
                                , title :: String
                                , completed :: Boolean
                                }
                            )
                        )
                  )

              )
        , "post" ::
            SchemaEndpoint
              "create_todo"
              (Just "create todo")
              ( EndpointSpec
                  -- The type of the path parameters 
                  -- (empty in this case)
                  ()
                  -- The type of the query parameters
                  -- (empty in this case)
                  ()
                  -- The type of the request body
                  ( Just
                      ( RequestBodySpec
                          "The create todo request body"
                          ( ApplicationJson
                              { title :: String
                              , completed :: Boolean
                              }
                          )
                      )
                  )
                  -- The type of the response body in the form of 
                  -- row of response content (status code in the key)
                  ( "200" ::
                      ResponseSpec
                        "The success response"
                        ( ApplicationJson
                            { id :: Int
                            , title :: String
                            , completed :: Boolean
                            }
                        )
                  )
              )
        )
  , "/api/todos/:id" ::
      SchemaEndpoints
        ( "get" ::
            SchemaEndpoint
              "get_todo_by_id"
              (Just "get todo by id")
              ( EndpointSpec
                  -- The type of the path parameters 
                  ( id :: Int
                  )
                  -- The type of the query parameters
                  -- (empty in this case)
                  ()
                  -- The type of the request body
                  -- (nothing in this case)
                  Nothing
                  -- The type of the response body in the form of 
                  -- row of response content (status code in the key)
                  ( "200" ::
                      ResponseSpec
                        "The success response"
                        ( ApplicationJson
                            { id :: Int
                            , title :: String
                            , completed :: Boolean
                            }
                        )
                  , "404" ::
                      ResponseSpec
                        "The not found response"
                        ( ApplicationJson { msg :: String }
                        )
                  )
              )
        )

  )