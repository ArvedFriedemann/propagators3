module PropagatorTypes where

import "lens" Control.Lens
import "lattices" Algebra.Lattice (Lattice)
import qualified "lattices" Algebra.Lattice as Lat
import "monad-parallel" Control.Monad.Parallel (MonadFork)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "mtl" Control.Monad.Reader
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique
import "monad-parallel" Control.Monad.Parallel (MonadFork, forkExec)
import "this" CustomVars

type Std m b a = (HasTop b, HasValue b a, Show b, Show a, HasProps m b a, Eq a, Lattice a)
--type Std m b = (HasTop b, Show b, Eq a)

class (forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a)) => StdPtr v
--instance (forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a)) => StdPtr v


class HasValue p a where
  value :: Lens' p a

class HasProps m p a where
  props :: Lens' p (PCollection m a)

class HasTop a where
  top :: a

class HasBot a where
  bot :: a


class HasTop a => HasDecTop a where
  isTop :: a -> Bool

class HasBot a => HasDecBot a where
  isBot :: a -> Bool

instance (Lat.BoundedMeetSemiLattice a) => HasTop a where
  top = Lat.top

instance (Lat.BoundedJoinSemiLattice a) => HasBot a where
  bot = Lat.bottom

type Scope = Int
type ScopePath = [Scope]

class (MonadFork m) => PropUtil m where
  getScope :: m Scope
  getScopePath :: m ScopePath
  inScope :: ScopePath -> m a -> m a
  inScope' :: Scope -> m a -> m a
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

falseToNoInst :: Bool -> Instantiated ()
falseToNoInst True = Instance ()
falseToNoInst False = NoInstance


--------------------------------------
--Propagator execution
--------------------------------------


data PropState m v =
  PS { scopes :: ScopePath, fixpointSem :: v (Int, [ReaderT (PropState m v) m ()])}

initPS :: forall v m . (MonadNew m v) => m (PropState m v)
initPS = do
  sem <- MV.new (0, [])
  return $ PS {scopes = [0], fixpointSem = sem}

runPropM :: forall v m a. (MonadVar m v, MonadIO m, MonadFork m) => ReaderT (PropState m v) m a -> m a
runPropM m = initPS @v >>= \s -> flip runReaderT s (incrementJobs >> m >>= \r -> decrementJobs >> return r)

-- type RWST r w s m a
-- is a reader with env r, writer with w, state with s, inner monad m

instance (MonadIO m, MonadFork m, MonadVar m v) => PropUtil (ReaderT (PropState m v) m) where
  getScope = asks (head . scopes)
  getScopePath = asks scopes
  scoped m = do
    u <- hash <$> (liftIO newUnique)
    local (\s -> s{scopes = u : scopes s}) m
  parScoped m = do
    s <- ask
    case scopes s of
      (_ : xs) -> do
        local (\s -> s{scopes = xs}) m
      _ -> error "calling parScoped on Parent!"
  inScope scp = local (\s -> s{scopes = scp})
  inScope' cp = local (\s -> s{scopes = cp : scopes s})
  incrementJobs = asks fixpointSem >>= \s -> lift $ MV.mutate_ s (\(i,l) -> (i + 1, l))
  decrementJobs = asks fixpointSem >>= (\s -> lift $ MV.mutate s (\(i,l) ->
    case i of
      1 -> ((length l,[]),l)
      _ -> ((i-1,l),[]))) >>= sequence_ . (map $ forkExec . (>> decrementJobs))
  -- addFixpoint :: ReaderT (PropState m v) m () -> ReaderT (PropState m v) m ()
  addFixpoint m = ask >>= \s -> lift $ MV.mutate_ (fixpointSem s) (\(i,l) -> (i,  (local (const s) m) : l))

class (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v) => MonadVar m v | m -> v
--instance (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v) => MonadVar m v

instance MonadVar IO UP
instance MonadVar (ReaderT (PropState IO UP) IO) UP
instance StdPtr UP
