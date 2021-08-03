module Class.MonadProp where

import "lattices" Algebra.Lattice (Lattice, BoundedMeetSemiLattice)
import qualified "lattices" Algebra.Lattice as Lat
import "base" Control.Monad.IO.Class
import "lens" Control.Lens
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV

import "this" Propagators hiding (new, write)
import qualified "this" Propagators as Prop
import "this" PropagatorTypes

type StdLat a = (Eq a, Show a, BoundedMeetSemiLattice a)

class (Monad m) => MonadProp m v where
  new :: (StdLat a) => m (v a)
  new' :: (StdLat a) => a -> m (v a)
  new' v = new >>= \p -> write p v >> return p

  readState :: (StdLat a) => v a -> m a

  iff :: (StdLat a) => v a -> (a -> Instantiated c) -> (c -> m ()) -> m ()
  iffm :: (StdLat a) => v a -> (a -> Maybe c) -> (c -> m ()) -> m ()
  iffm ptr fkt = iff ptr (nothingToNoInst . fkt)
  readUpdate :: (StdLat a) => v a -> (a -> m ()) -> m ()
  readUpdate v = iff v ContinuousInstance
  watch :: (StdLat a) => v a -> m () -> m ()
  watch v m = readUpdate v (const m)

  write :: (StdLat a) => v a -> a -> m ()

  merge :: (StdLat a) => v a -> v a -> m ()

  scoped :: m a -> m a
  parScoped :: m a -> m a

  watchFixpoint :: m () -> m ()

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

instance (forall k. Eq (v k), MonadVar m v, PropUtil m, Monad m) => MonadProp m (CustPtr m v) where
  new = Prop.new >>= return . CustPtr
  readState (CustPtr p) = readLens value p
  iff (CustPtr p) = addPropagator p
  write (CustPtr p) = writeLens value p
  merge (CustPtr p1) (CustPtr p2) = mergePtrs p1 p2
  scoped = PropagatorTypes.scoped
  parScoped = PropagatorTypes.parScoped
  watchFixpoint = addFixpoint
