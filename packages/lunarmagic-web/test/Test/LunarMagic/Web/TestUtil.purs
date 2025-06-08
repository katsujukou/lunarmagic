module Test.LunarMagic.Web.TestUtil where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception as Exn
import Test.Spec.Assertions (shouldEqual)

shouldReturn :: forall m a. MonadThrow Exn.Error m => MonadEffect m => Show a => Eq a => Effect a -> a -> m Unit
shouldReturn ml r = do
  l <- liftEffect ml
  l `shouldEqual` r
