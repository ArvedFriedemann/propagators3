module Temp where
{-}
import "base" Debug.Trace

class A x where
  t :: ()
class BOT

instance {-# INCOHERENT #-} A x where
  t = trace "First Instance" ()
instance {-# INCOHERENT #-} BOT => A Int where
  t = trace "Second Instance" ()

f :: A Int => ()
f = t
-}
