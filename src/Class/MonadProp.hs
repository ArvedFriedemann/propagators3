module Class.MonadProp where

import "lattices" Algebra.Lattice (Lattice, BoundedMeetSemiLattice)
import qualified "lattices" Algebra.Lattice as Lat
import "base" Control.Monad.IO.Class
import "lens" Control.Lens

import "this" Propagators
import "this" PropagatorTypes

class MonadProp m v where
  readState :: v a -> m a
  readUpdate :: (Eq a, Lattice a) => v a -> (a -> m b) -> m ()
  watch :: (Eq a, Lattice a) => v a -> m b -> m ()
  watch v m = readUpdate v (const m)
  write :: (Eq a, Lattice a) => v a -> a -> m ()

  merge :: (Eq a, Lattice a) => v a -> v a -> m ()

  scoped :: m a -> m a
  parScoped :: m a -> m a

  watchFixpoint :: m a -> m a

newtype PtrCont m a = PtrCont (a, PCollection m a)

ptrContLens :: Lens' (PtrCont m a) (a, PCollection m a)
ptrContLens = lens (\(PtrCont c) -> c) (\(PtrCont _) c -> PtrCont c)

instance (a~b) => HasValue (PtrCont m a) b where
  value = ptrContLens . _1

instance (a~b) => HasProps m (PtrCont m a) b where
  props = ptrContLens . _2

instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []

instance (MonadIO m) => MonadProp m (PtrCont m) where
