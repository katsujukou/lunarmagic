module Test.LunarMagic.Web.URL where

import Prelude

import Data.Either (fromRight', isRight)
import Data.Maybe (Maybe(..), isJust)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Exception (throw)
import Effect.Exception as Exn
import Effect.Unsafe (unsafePerformEffect)
import Fmt as Fmt
import LunarMagic.Web.URL (canParseFn)
import LunarMagic.Web.URL as URL
import Partial.Unsafe (unsafeCrashWith)
import Test.LunarMagic.Web.TestUtil (shouldReturn)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

spec :: Spec Unit
spec = describe "module LunarMagic.Web.URL" do
  let
    protocol = "http:"
    host = "example.com"
    pathname = "/path/to/there"
    search = "?foo=42&bar&baz=qux&baz=quux&baz=quuux"

    href = Fmt.fmt
      @"{protocol}//{host}{pathname}{search}"
      { protocol, host, pathname, search }

    eurl = unsafePerformEffect $ Exn.try $ URL.new href

  it "should success to make URL" do
    eurl `shouldSatisfy` isRight

  let
    url = fromRight' (\_ -> unsafeCrashWith "impossible") eurl

  it "should return correct protocol" do
    URL.protocol url `shouldReturn` protocol

  it "should return correct host" do
    URL.host url `shouldReturn` host

  it "should return correct pathname" do
    URL.pathname url `shouldReturn` pathname

  it "should return correct search string" do
    URL.search url `shouldReturn` search

  it "should return the whole url" do
    URL.href url `shouldReturn` href

  it "should be converted back to whole url string" do
    URL.toString url `shouldReturn` href

  it "should be converted back to whole url string" do
    URL.toString url `shouldReturn` href

  it "should behave same as `toString`" do
    URL.toJSON url `shouldReturn` href

  it "should return true for valid URL string" do
    canParseFn href `shouldEqual` true

  it "should succeed to parse" do
    let parsedURL = URL.parse href
    parsedURL `shouldSatisfy` isJust
    case parsedURL of
      Nothing -> liftEffect $ throw "impossible"
      Just parsed -> do
        URL.toString parsed `shouldReturn` href