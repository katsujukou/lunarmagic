module LunarMagic.Web.Headers.Init where


import Foreign.Object (Object)
import Type.Row.Homogeneous (class Homogeneous)
import Unsafe.Coerce (unsafeCoerce)

foreign import data HeadersInit :: Type 

unsafeFromArray :: Array (Array String) -> HeadersInit 
unsafeFromArray = unsafeCoerce

fromObject :: Object (Array String) -> HeadersInit 
fromObject = unsafeCoerce

fromRecord :: forall r. Homogeneous r String => Record r -> HeadersInit 
fromRecord = unsafeCoerce