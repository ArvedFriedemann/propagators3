module PropagatorTypes where

import "lens" Control.Lens
import "lattices" Algebra.Lattice (Lattice)
import qualified "lattices" Algebra.Lattice as Lat
import "monad-parallel" Control.Monad.Parallel (MonadFork)

type Std m b a = (HasTop b, HasValue b a, Show b, Show a, HasProps m b a, Eq a, Lattice a)
--type Std m b = (HasTop b, Show b, Eq a)

class (forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a)) => StdPtr v
instance (forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a)) => StdPtr v


class HasValue p a where
  value :: Lens' p a

class HasProps m p a where
  props :: Lens' p (PCollection m a)

class HasTop a where
  top :: a

class HasBot a where
  bot :: a


class HasDecTop a where
  isTop :: a -> Bool

class HasDecBot a where
  isBot :: a -> Bool

instance (Lat.BoundedMeetSemiLattice a) => HasTop a where
  top = Lat.top

instance (Lat.BoundedJoinSemiLattice a) => HasBot a where
  bot = Lat.bottom

type ScopePath = [Int]

class (MonadFork m) => PropUtil m where
  getScope :: m Int
  getScopePath :: m ScopePath
  scoped :: m a -> m a
  parScoped :: m a -> m a
  incrementJobs :: m ()
  decrementJobs :: m ()
  addFixpoint :: m () -> m ()

data ContRec m a = forall b. ContRec {
  crpred :: a -> Instantiated b,
  crcont :: b -> m ()
}

instance Show (ContRec m a) where
  show _ = "~ContRec~"

type PCollection m a = [ContRec m a]

data Instantiated a = Failed | NoInstance | Instance a | ContinuousInstance a
  deriving (Show, Eq, Ord)

nothingToNoInst :: Maybe a -> Instantiated a
nothingToNoInst (Just c) = Instance c
nothingToNoInst Nothing = NoInstance

nothingToFailed :: Maybe a -> Instantiated a
nothingToFailed (Just c) = Instance c
nothingToFailed Nothing = Failed
