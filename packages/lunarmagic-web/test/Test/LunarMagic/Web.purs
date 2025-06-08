module Test.LunarMagic.Web where

import Prelude

import Effect (Effect)
import Test.LunarMagic.Web.Request as Request
import Test.LunarMagic.Web.URL as URL
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  URL.spec
  Request.spec