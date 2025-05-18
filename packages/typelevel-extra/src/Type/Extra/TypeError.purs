module Type.Extra.TypeError where

import Prim.Boolean (False, True)
import Prim.TypeError (class Fail, Above, Beside, Doc)

infixl 6 type Beside as ++
infixl 5 type Above as |>

class FailWhen :: Boolean -> Doc -> Constraint
class FailWhen b doc

instance FailWhen False doc
instance Fail doc => FailWhen True doc