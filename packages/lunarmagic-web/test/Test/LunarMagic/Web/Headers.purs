module Test.LunarMagic.Web.Headers where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Foreign.Object as Object
import LunarMagic.Web.Headers (Headers)
import LunarMagic.Web.Headers as Headers
import Test.LunarMagic.Web.TestUtil (shouldNotThrowEffect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "module LunarMagic.Web.Headers" do
  describe "introduction" do
    it "should make empty headers" do
      Headers.new (Headers.fromObject (Object.empty)) # shouldNotThrowEffect
      Headers.new (Headers.fromRecord {}) # shouldNotThrowEffect
      Headers.new (Headers.unsafeFromArray []) # shouldNotThrowEffect

    it "should make headers from object" do
      let
        obj = Object.fromFoldable
          [ "Content-Type" /\ [ "application/json", "application/xml" ]
          , "X-Requested-With" /\ [ "affjax" ]
          ]
      Headers.new (Headers.fromObject obj) # shouldNotThrowEffect

    it "should make headers from record" do
      let
        rec =
          { "Content-Type": [ "text/plain" ]
          , "Access-Control-Allow-Origin": [ "*" ]
          , "Access-Control-Allow-Headers": [ "Content-Type", "Authorization", "X-Requested-With" ]
          , "Access-Control-Allow-Methods": [ "GET", "HEAD", "POST", "PUT", "PATCH", "DELETE" ]
          }
      Headers.new (Headers.fromRecord rec) # shouldNotThrowEffect

  describe "elimination" do
    let 
      singleton k v= liftEffect do
        h <- Headers.new (Headers.fromRecord {})
        h # Headers.append k v
        pure h 

    it "should append header" do
      h <- singleton "Content-Type" "application/json"     
      h `shouldContains` ("Content-Type" /\ ["application/json"])
      liftEffect $ h # Headers.append "Content-Type" "application/xml"
      h `shouldContains` ("Content-Type" /\ ["application/json", "application/xml"])

    it "should set header" do
      h <- singleton "Content-Type" "application/json"    
      h `shouldContains` ("Content-Type" /\ ["application/json"])
      liftEffect $ h # Headers.set "Content-Type" "application/xml"
      h `shouldContains` ("Content-Type" /\ ["application/xml"])

    it "should delete header" do
      h <- singleton "Content-Type" "application/json"
      liftEffect $ h # Headers.delete "Content-Type"
      b <- liftEffect $ h # Headers.has "Content-Type"
      b `shouldEqual` false

shouldContains :: forall m. MonadEffect m => MonadThrow Exn.Error m => Headers -> Tuple String (Array String) -> m Unit
shouldContains headers (key /\ vs) = do
  hs <- liftEffect $ fromMaybe "" <$> Headers.get key headers 
  (String.trim <$> String.split (Pattern ",") hs) 
    `shouldEqual` vs