module Test.LunarMagic.Web.Request where

import Prelude

import Data.Argonaut.Core (stringify)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Either (Either(..))
import Data.Maybe (fromJust)
import Data.Nullable as Nullable
import Data.String as String
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import LunarMagic.Web.Request (All, Request)
import LunarMagic.Web.Request as Request
import LunarMagic.Web.URL as URL
import Partial.Unsafe (unsafePartial)
import Test.LunarMagic.Web.TestUtil (shouldReturn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)

type Method = String

foreign import newRequest :: forall a. EffectFn3 Method String a (Request All)

spec :: Spec Unit
spec = describe "module LunarMagic.Web.Request" do
  let
    newGetRequest :: String -> Effect (Request All)
    newGetRequest url = runEffectFn3 newRequest "get" url Nullable.null

    newPostRequest :: forall a. String -> a -> Effect (Request All)
    newPostRequest url body = runEffectFn3 newRequest "post" url body

  it "should return correct method" do
    req <- liftEffect $ newPostRequest "http://localhost" {}
    (String.toLower <$> Request.method req) `shouldReturn` "post"

  it "should be able to get url" do
    let
      urls =
        [ "http://localhost"
        , "http://localhost/foo"
        , "http://localhost/foo?qux=42&quux=nice"
        ]
    for_ urls \urlString -> do
      let parsedUrl = unsafePartial $ fromJust (URL.parse urlString)
      req <- liftEffect $ newGetRequest urlString
      reqUrl <- liftEffect $ Request.url req
      URL.href parsedUrl `shouldReturn` reqUrl

  it "should extract body as string" do
    req <- liftEffect $ newPostRequest "http://localhost" "Hello"
    bodyText <- Request.text req
    bodyText `shouldEqual` "Hello"

  it "should extract body as json" do
    let
      testRequestBody =
        { foo: 42
        , bar: "Hello"
        , baz: [ true, true, false ]
        }
      testRequestBodyCodec =
        CA.object "TestRequestBody" $
          CAR.record
            { foo: CA.int
            , bar: CA.string
            , baz: CA.array CA.boolean
            }
    req <- liftEffect $ newPostRequest "http://localhot" (stringify $ CA.encode testRequestBodyCodec testRequestBody)
    jsonBody <- Request.json req
    let decodedBody = CA.decode testRequestBodyCodec (unsafeCoerce jsonBody)
    decodedBody `shouldEqual` Right testRequestBody