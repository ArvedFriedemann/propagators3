module RunPropagators where

import "this" Propagators
import "this" LensVars
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "lattices" Algebra.Lattice
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "monad-var" MonadVar.Instances.IORef
import "base" Data.IORef
import "lens" Control.Lens

type PtrCont m a = (a,PCollection m a)

instance (a~b) => HasValue (PtrCont m a) b where
  value = _1

instance (a~b) => HasProps m (PtrCont m a) b where
  props = _2

instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []

newLens' :: forall v a m . (BoundedMeetSemiLattice a, MonadNew m v) =>
  a -> m (PtrType v (PtrCont m a))
newLens' = newLens value

test :: forall v. (v ~ IORef) => IO ()
test = do
  v1 <- newLens' @v ["a"]
  v2 <- newLens' @v []
  merge v1 v2
  iff v2 (\v -> if null v then NoInstance else Instance) (putStrLn . show)
  return ()
