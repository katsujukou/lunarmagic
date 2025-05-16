module Example.HelloWorld where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console

main :: Effect Unit
main = do
  Console.log "This is HelloWorld example."