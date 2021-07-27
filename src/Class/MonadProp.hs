module Class.MonadProp where

import "lattices" Algebra.Lattice (Lattice, BoundedMeetSemiLattice)
import qualified "lattices" Algebra.Lattice as Lat
import "base" Control.Monad.IO.Class
import "lens" Control.Lens
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV

import "this" Propagators
import "this" PropagatorTypes

class MonadProp m v where
  readState :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> m a

  iff :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> (a -> Instantiated) -> (a -> m ()) -> m ()
  readUpdate :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> (a -> m ()) -> m ()
  readUpdate v = iff v (const ContinuousInstance)
  watch :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> m () -> m ()
  watch v m = readUpdate v (const m)

  write :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> a -> m ()

  merge :: (Eq a, Show a, BoundedMeetSemiLattice a) => v a -> v a -> m ()

  scoped :: m a -> m a
  parScoped :: m a -> m a

  watchFixpoint :: m a -> m a

type PtrCont m a = (a, PCollection m a)
newtype CustPtr m v a = CustPtr (PtrType v (PtrCont m a))
deriving instance (forall k. Eq (v k)) => Eq (CustPtr m v a)

instance (a~b) => HasValue (PtrCont m a) b where
  value = _1

instance (a~b) => HasProps m (PtrCont m a) b where
  props = _2

instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []

instance (forall k. Eq (v k), MonadVar m v, PropUtil m) => MonadProp m (CustPtr m v) where
  readState (CustPtr p) = readLens value p
  iff (CustPtr p) = addPropagator p
  write (CustPtr p) = writeLens value p
  merge (CustPtr p1) (CustPtr p2) = mergePtrs p1 p2
