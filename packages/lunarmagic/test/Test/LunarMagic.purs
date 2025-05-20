module Test.LunarMagic where

import Prelude

import Effect (Effect)
import Partial.Unsafe (unsafeCrashWith)
import Prim.Row as Row
import Test.Spec (it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Unsafe.Coerce (unsafeCoerce)

newtype Response = Response { body :: String }

foreign import data ResponseBody :: Type -> Type

newResponse :: forall body. ResponseBody body -> Response
newResponse = unsafeCoerce

newtype ServerM :: forall k. k -> Type -> Type
newtype ServerM r a = ServerM a

instance Functor (ServerM r) where
  map _ _ = unsafeCrashWith "NotImplemented"

instance Apply (ServerM r) where
  apply _ _ = unsafeCrashWith "NotImplemented"

instance Bind (ServerM r) where
  bind _ _ = unsafeCrashWith "NotImplemented"

instance Applicative (ServerM r) where
  pure _ = unsafeCrashWith "NotImplemented"

instance Monad (ServerM r)

abort :: forall @code a body resps r. Row.Cons code body r resps => ResponseBody body -> ServerM resps a
abort = unsafeCoerce

ok :: forall r body. ResponseBody body -> ServerM ("200" :: body | r) Response
ok = unsafeCoerce

badRequest :: forall r body. ResponseBody body -> ServerM ("400" :: body | r) Response
badRequest = unsafeCoerce

string :: String -> ResponseBody String
string = unsafeCoerce

json :: forall t. t -> ResponseBody t
json = unsafeCoerce

type Request ctx = { | ctx }

foreign import data Router :: Row Type -> Type

get :: forall @path @spec resps. (Request spec -> ServerM resps Response) -> Router resps
get = unsafeCrashWith "Not implemented"

handleHello
  :: Request
       ( params ::
           { name :: String
           }
       )
  -> ServerM
       ( "200" :: String
       , "400" ::
           { error :: String
           }
       )
       Response
handleHello req = do
  when (req.params.name == "") do
    abort @"400" $ json { error: "You must specify name!" }

  ok $ string $ "Hello, World!"

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  it "should be added some test suits" do
    42 `shouldEqual` (40 + 2)