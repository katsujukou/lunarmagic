module Test.LunarMagic.Server.ServerM where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit 
spec = describe "LunarMagic.Server.ServerM" do
  it "should pass" do
    42 `shouldEqual` (40 + 2)
    