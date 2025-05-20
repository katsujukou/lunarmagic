module LunarMagic.Server.ServerM.CallStack
  ( CallStack
  , WithCallStack
  , annotate
  , callstack
  , unwrap
  )
  where

import Prelude

import Data.String (joinWith)

newtype CallStack = CallStack (Array String)

instance showCallStack :: Show CallStack where
  show (CallStack cs) = "callstack:\n" <> joinWith "\n" cs

foreign import current :: CallStack

newtype WithCallStack a = WithCallStack
  { it :: a
  , stack :: CallStack
  }

instance eqWithCallstack :: Eq a => Eq (WithCallStack a)
  where
  eq (WithCallStack { it: a }) (WithCallStack { it: b }) = a == b

instance ordWithCallstack :: Ord a => Ord (WithCallStack a)
  where
  compare (WithCallStack { it: a }) (WithCallStack { it: b }) = a `compare` b

instance showWithCallstack :: Show a => Show (WithCallStack a) where
  show (WithCallStack { it: a }) = show a

instance functorWithCallstack :: Functor WithCallStack where
  map f (WithCallStack wcs) = WithCallStack (wcs { it = f wcs.it })

callstack :: forall a. WithCallStack a -> CallStack 
callstack (WithCallStack { stack }) = stack 
  
unwrap :: forall a. WithCallStack a -> a
unwrap (WithCallStack { it }) = it

annotate :: forall a. a -> WithCallStack a
annotate = WithCallStack <<< { it: _, stack: current }