module Example.HelloWorld where

import Prelude

import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Random (randomBool)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import LunarMagic.Server.Handler.Eval (EvalResult(..), eval)
import LunarMagic.Server.Handler.Spec (ApiHandlerSpec)
import LunarMagic.Server.ServerF (ServerM, send)
import LunarMagic.Server.ServerM.CallStack as CallStack
import LunarMagic.Web.Request as Web
import LunarMagic.Web.Response (Response)
import Promise (Promise)
import Promise.Aff (fromAff)

type Spec = ApiHandlerSpec
  ( responses ::
    { "200" :: String }
  )

echoHandler :: forall ctx e m. MonadEffect m => ServerM Spec ctx e m Response 
echoHandler = do
  Console.log "Invoked"
  whenM (liftEffect randomBool) do
    Console.log "T"
    send @200 "Oops!"
  Console.log "F"
  send @200 "Hello!"

fetch :: EffectFn1 Web.Request (Promise Response)
fetch = mkEffectFn1 $ fromAff <<< fetchAff
  where 
  fetchAff :: Web.Request -> Aff Response
  fetchAff request = eval { request } echoHandler >>= case _ of 
    Done resp -> pure resp
    Sended resp -> pure resp 
    Thrawn e -> absurd $ CallStack.unwrap e 
