module Rec where

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

shrink :: forall r1 r2 r3. Union r1 r2 r3 => { | r3 } -> { | r1 }
shrink = unsafeCoerce