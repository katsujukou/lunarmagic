module LunarMagic.Server.Handler.Eval
  ( EvalResult(..)
  , eval
  )
  where

import Prelude

import Control.Applicative.Free (hoistFreeAp, retractFreeAp)
import Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import Control.Monad.Free (foldFree)
import Data.Either (Either(..))
import Effect.Aff (Aff, parallel, sequential)
import LunarMagic.Server.Context (Context)
import LunarMagic.Server.ServerF (ServerAp(..), ServerF(..), ServerM(..))
import LunarMagic.Server.ServerM.CallStack (WithCallStack)
import LunarMagic.Web.Response (Response)

eval
  :: forall spec ctx e a
   . Context ctx
  -> ServerM spec ctx e Aff a
  -> Aff (EvalResult e a)
eval ctx m = runExceptT (eval' ctx m) >>= case _ of 
    Right a -> pure (Done a)
    Left (Right resp) -> pure (Sended resp)
    Left (Left e) -> pure (Thrawn e)

data EvalResult e a 
  = Sended Response
  | Thrawn (WithCallStack e) 
  | Done a 

eval' :: forall spec ctx e a. Context ctx -> ServerM spec ctx e Aff a -> ExceptT (Either (WithCallStack e) Response) Aff a
eval' ctx (ServerM m) = foldFree (go ctx) m
  where
  go
    :: forall spec' ctx' e' a'
     . Context ctx'
    -> ServerF spec' ctx' e' Aff a'
    -> ExceptT (Either (WithCallStack e') Response) Aff a' 
  go ctx' = case _ of
    GetContext reply -> pure (reply ctx')
    Lift m' -> lift m'
    Throw e -> throwError (Left e)
    Send resp -> throwError (Right resp)
    Par (ServerAp p) -> sequential $ retractFreeAp $ hoistFreeAp (parallel <<< eval' ctx') p