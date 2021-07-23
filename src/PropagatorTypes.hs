module PropagatorTypes where

import "lens" Control.Lens
import "lattices" Algebra.Lattice (Lattice)
import qualified "lattices" Algebra.Lattice as Lat
import "monad-parallel" Control.Monad.Parallel (MonadFork)

type Std m b a = (HasTop b, HasValue b a, Show b, Show a, HasProps m b a, Eq a, Lattice a)
--type Std m b = (HasTop b, Show b, Eq a)

class HasValue p a where
  value :: Lens' p a

class HasProps m p a where
  props :: Lens' p (PCollection m a)

class HasTop a where
  top :: a

instance (Lat.BoundedMeetSemiLattice a) => HasTop a where
  top = Lat.top

type ScopePath = [Int]

class (MonadFork m) => PropUtil m where
  getScope :: m Int
  getScopePath :: m ScopePath
  scoped :: m a -> m a
  parScoped :: m a -> m a
  incrementJobs :: m ()
  decrementJobs :: m ()
  addFixpoint :: m () -> m ()

data ContRec m a = ContRec {
  crpred :: a -> Instantiated,
  crcont :: m ()
}

instance Show (ContRec m a) where
  show _ = "~ContRec~"

type PCollection m a = [ContRec m a]

data Instantiated = Failed | NoInstance | Instance | ContinuousInstance
  deriving (Show, Eq, Ord)
