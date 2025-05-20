module LunarMagic.Server.ServerF where

import Prelude

import Control.Applicative.Free (FreeAp, hoistFreeAp, liftFreeAp)
import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Parallel (class Parallel)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, over)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import LunarMagic.HttpStatus.Types (class FromStatusCode, class StatusCodeInt, statusCodeInt)
import LunarMagic.Server.Context (Context)
import LunarMagic.Server.Handler.Response (class ToBody, toBody)
import LunarMagic.Server.Handler.Spec (class SpecResponses, HandlerSpec)
import LunarMagic.Server.Headers (Headers)
import LunarMagic.Server.Headers as Headers
import LunarMagic.Server.ServerM.CallStack (WithCallStack)
import LunarMagic.Server.ServerM.CallStack as CallStack
import LunarMagic.Web.Response (Response)
import LunarMagic.Web.Response as Response
import Prim.Row as Row

data ServerF spec ctx e m a
  = GetContext (Context ctx -> a)
  | Lift (m a)
  | Send Response
  | Throw (WithCallStack e)
  | Par (ServerAp spec ctx e m a)

instance functorServerF :: Functor m => Functor (ServerF spec ctx e m) where
  map f = case _ of
    GetContext reply -> GetContext (f <<< reply)
    Lift m -> Lift (map f m)
    Throw e -> Throw e
    Send resp -> Send resp
    Par pa -> Par (map f pa)

newtype ServerM :: HandlerSpec -> Row Type -> Type -> (Type -> Type) -> Type -> Type
newtype ServerM spec ctx e m a = ServerM (Free (ServerF spec ctx e m) a)

derive newtype instance functorServerM :: Functor (ServerM spec ctx e m)
derive newtype instance applyServerM :: Apply (ServerM spec ctx e m)
derive newtype instance applicativeServerM :: Applicative (ServerM spec ctx e m)
derive newtype instance bindServerM :: Bind (ServerM spec ctx e m)
derive newtype instance monadServerM :: Monad (ServerM spec ctx e m)
derive newtype instance semigroupServerM :: Semigroup a => Semigroup (ServerM spec ctx e m a)
derive newtype instance monoidServerM :: Monoid a => Monoid (ServerM spec ctx e m a)

instance monadEffectServerM :: MonadEffect m => MonadEffect (ServerM spec ctx e m) where
  liftEffect = ServerM <<< liftF <<< Lift <<< liftEffect

instance monadAffServerM :: MonadAff m => MonadAff (ServerM spec ctx e m) where
  liftAff = ServerM <<< liftF <<< Lift <<< liftAff

instance parallelServerApServerM :: Parallel (ServerAp spec ctx e m) (ServerM spec ctx e m) where
  parallel = ServerAp <<< liftFreeAp
  sequential = ServerM <<< liftF <<< Par

instance monadThrowServerM :: MonadThrow (WithCallStack e) (ServerM spec ctx e m) where
  throwError = ServerM <<< liftF <<< Throw

getContext :: forall spec ctx e m. ServerM spec ctx e m (Context ctx)
getContext = ServerM $ liftF $ GetContext identity

throw :: forall spec ctx e m a. e -> ServerM spec ctx e m a
throw = ServerM <<< liftF <<< Throw <<< CallStack.annotate

send'
  :: forall @status statusCode spec resps body ctx e m a _1
   . SpecResponses spec (Record resps)
  => FromStatusCode status statusCode
  => StatusCodeInt status
  => Row.Cons statusCode body _1 resps
  => ToBody body
  => MonadEffect m 
  => Headers
  -> body
  -> ServerM spec ctx e m a
send' headers b = ServerM <<< liftF <<< Send =<< liftEffect resp
  where
    resp = Response.new (Just $ toBody b) 
      { headers: Headers.toWeb headers 
      , status: statusCodeInt @status
      , statusText: ""
      }

send
  :: forall @status statusCode spec resps body ctx e m a _1
   . SpecResponses spec (Record resps)
  => FromStatusCode status statusCode
  => StatusCodeInt status
  => Row.Cons statusCode body _1 resps
  => ToBody body
  => MonadEffect m 
  => body
  -> ServerM spec ctx e m a
send = send' @status Headers.empty

newtype ServerAp :: HandlerSpec -> Row Type -> Type -> (Type -> Type) -> Type -> Type
newtype ServerAp spec ctx e m a = ServerAp (FreeAp (ServerM spec ctx e m) a)

derive instance newtypeServerAp :: Newtype (ServerAp spec ctx e m a) _
derive newtype instance functorServerAp :: Functor (ServerAp sepc ctx e m)
derive newtype instance applyServerAp :: Apply (ServerAp sepc ctx e m)
derive newtype instance applicativeServerAp :: Applicative (ServerAp sepc ctx e m)

hoist :: forall spec ctx e m n a. (m ~> n) -> ServerM spec ctx e m a -> ServerM spec ctx e n a
hoist nat (ServerM m) = ServerM (hoistFree go m)
  where
  go :: ServerF spec ctx e m ~> ServerF spec ctx e n
  go = case _ of
    GetContext reply -> GetContext reply
    Lift m' -> Lift (nat m')
    Throw e -> Throw e
    Send resp -> Send resp
    Par p -> Par (over ServerAp (hoistFreeAp (hoist nat)) p)

mapError :: forall spec ctx e1 e2 m a. (e1 -> e2) -> ServerM spec ctx e1 m a -> ServerM spec ctx e2 m a 
mapError f (ServerM m) = ServerM (hoistFree go m)
  where
    go :: ServerF spec ctx e1 m ~> ServerF spec ctx e2 m 
    go = case _ of 
      GetContext reply -> GetContext reply
      Throw e -> Throw (map f e)
      Lift m' -> Lift m' 
      Send resp -> Send resp
      Par p -> Par (over ServerAp (hoistFreeAp (mapError f)) p)
