module Class.MonadProp where

import "lattices" Algebra.Lattice (Lattice, BoundedMeetSemiLattice)
import qualified "lattices" Algebra.Lattice as Lat
import "base" Control.Monad.IO.Class
import "lens" Control.Lens
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S

import "this" Propagators hiding (new, write, parScoped)
import qualified "this" Propagators as Prop
import "this" PropagatorTypes hiding (parScoped)
import qualified "this" PropagatorTypes as PropT

type StdLat a = (Eq a, Show a, BoundedMeetSemiLattice a)

class (Monad m, StdPtr v) => MonadProp m v | m -> v where
  new :: (StdLat a) => m (v a)
  new' :: (StdLat a) => a -> m (v a)
  new' v = new >>= \p -> write p v >> return p

  getCurrScopePtr :: (StdLat a) => v a -> m (v a)

  readState :: (StdLat a) => v a -> m a

  iff :: (StdLat a) => v a -> (a -> Instantiated c) -> (c -> m ()) -> m ()

  write :: (StdLat a) => v a -> a -> m ()

  merge :: (StdLat a) => v a -> v a -> m ()

  scoped :: m a -> m a
  parScoped :: m a -> m a

  watchFixpoint :: m () -> m ()



  iffm :: (StdLat a) => v a -> (a -> Maybe c) -> (c -> m ()) -> m ()
  iffm ptr fkt = iff ptr (nothingToNoInst . fkt)

  iffb :: (StdLat a) => v a -> (a -> Bool) -> m () -> m ()
  iffb ptr fkt m = iff ptr (falseToNoInst . fkt) (const m)

  readUpdate :: (StdLat a) => v a -> (a -> m ()) -> m ()
  readUpdate v = iff v ContinuousInstance

  watch :: (StdLat a) => v a -> m () -> m ()
  watch v m = readUpdate v (const m)

  promote :: (StdLat a) => v a -> m ()
  promote p = readUpdate p (\v -> parScoped @_ @v $ write p v)

  dirEq :: (StdLat a) => v a -> v a -> m ()
  dirEq p1 p2 = readUpdate p1 (\v -> write p2 v)

type PtrCont m a = (a, PCollection m a)
newtype CustPtr m v a = CustPtr (PtrType v (PtrCont m a))
deriving instance (forall k. Eq (v k)) => Eq (CustPtr m v a)
deriving instance (forall k. Show (v k)) => Show (CustPtr m v a)
deriving instance (forall k. Ord (v k)) => Ord (CustPtr m v a)

instance (forall k. Eq (v k), forall k. Show (v k), forall k. Ord (v k)) => StdPtr (CustPtr m v)

instance (a~b) => HasValue (PtrCont m a) b where
  value = _1

instance (a~b) => HasProps m (PtrCont m a) b where
  props = _2

instance Lattice (PCollection m a) where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice (PCollection m a) where
  top = []

instance HasDecTop (PCollection m a) where
  isTop = null

{-}
instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []

instance HasDecTop [a] where
  isTop = null

-}

newtype RevSet a = RS (Set a)
  deriving (Show, Eq, Ord) via (Set a)

instance (Ord a) => Lattice (RevSet a) where
  (RS s1) /\ (RS s2) = RS (S.union s1 s2)
  (RS s1) \/ (RS s2) = RS (S.intersection s1 s2)

instance (Ord a) => BoundedMeetSemiLattice (RevSet a) where
  top = RS S.empty

instance (StdPtr v, MonadVar m v, PropUtil m, Monad m) => MonadProp m (CustPtr m v) where
  new = Prop.new >>= return . CustPtr
  getCurrScopePtr (CustPtr p) = CustPtr <$> deRefRaw p
  readState (CustPtr p) = readLens value p
  iff (CustPtr p) = addPropagator p
  write (CustPtr p) = Prop.write p
  merge (CustPtr p1) (CustPtr p2) = mergePtrs p1 p2
  scoped = PropT.scoped
  parScoped = PropT.parScoped
  watchFixpoint = addFixpoint
