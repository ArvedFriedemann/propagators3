{-# LANGUAGE NoImplicitPrelude #-}
module Propagators where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
--import "mtl" Control.Monad.Except
import "base" Data.List
import "base" Control.Monad
import "monad-parallel" Control.Monad.Parallel (MonadFork, forkExec)


type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v)

class HasValIn p a where
  getValue :: p -> a
  setValue :: a -> p -> p

class (HasValIn p a) => HasValue p a where
  getVal :: p -> a
  getVal = getValue
  setVal :: a -> p -> p
  setVal = setValue

class (HasValIn k (PCollection m a)) => HasProps m k a where
  getProps :: k -> PCollection m a
  getProps = getValue
  setProps :: PCollection m a -> k -> k
  setProps = setValue

class HasEmpty k where
  empty :: k

write :: forall m v k a b.
  (MonadFork m, Dereference m v, MonadMutate m v, HasValue b a, HasValue (FEither v b) a, HasProps m b a, HasProps m (FEither v b) a, Eq a, Lattice a) =>
  v (FEither v b) -> a -> m ()
write adr' val = deRef adr' >>= \adr -> MV.mutate adr (\v -> updateVal @_ @_ @_ @a (setVal (getVal v /\ val) v)) >>= runProps


addPropagator :: (Dereference m v, MonadMutate m v, HasValue b a, HasValue (FEither v b) a, HasProps m b a, HasProps m (FEither v b) a) =>
  v (FEither v b) -> (a -> Instantiated) -> (a -> m ()) -> m ()
addPropagator p' pred cont =
  join $ deRef p' >>= \p -> MV.mutate p $ \v->  case pred (getVal v) of
      Failed -> (v, return ())
      Instance -> (v, cont $ getVal v)
      NoInstance -> (setProps (ContRec pred (read p >>= cont) : getProps v) v, return ())

--second collection is the succeeding propagators, first is the failed one that needs to be written back
notifyPure :: a -> PCollection m a -> (PCollection m a, PCollection m a)
notifyPure val props = (inst, noInst)
  where (_, noInst, inst) = splitInstantiated props (($ val) . crpred)

updateVal :: forall m v k a b. (HasValue b a, HasValue (FEither v b) a, HasProps m b a, HasProps m (FEither v b) a)
  => (FEither v b) -> ((FEither v b), PCollection m a)
updateVal mt = (setProps nosuccprops mt, succprops)
  where --TODO: equality check with old value?
    (nosuccprops, succprops) = notifyPure (getVal mt) (getProps mt)

runProps :: (MonadFork m) => PCollection m a -> m ()
runProps = mapM_ (forkExec . crcont)

data ContRec m a = ContRec {
  crpred :: a -> Instantiated,
  crcont :: m ()
}



type PCollection m a = [ContRec m a]

data Instantiated = Failed | NoInstance | Instance
  deriving (Show, Eq, Ord)

splitInstantiated :: [a] -> (a -> Instantiated) -> ([a],[a],[a])
splitInstantiated lst f = (filter ((== Failed) . f) lst
                          ,filter ((== NoInstance) . f) lst
                          ,filter ((== Instance) . f) lst)

iff :: (Dereference m v, MonadMutate m v, HasValue b a, HasValue (FEither v b) a, HasProps m b a, HasProps m (FEither v b) a) =>
  v (FEither v b) -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff p pred m = addPropagator p pred m


-----------------------------------
--Pointer merging
-----------------------------------

newtype FEither v a = FEither (Either (v (FEither v a)) a)

instance Lattice a => Lattice (FEither v a) where
  (FEither (Right v1)) /\ (FEither (Right v2)) = FEither $ Right $ v1 /\ v2
  (FEither (Right v1)) \/ (FEither (Right v2)) = FEither $ Right $ v1 \/ v2

class Dereference m v where
  deRef :: v (FEither v a) -> m (v (FEither v a))

merge :: forall m v k a b. (Dereference m v, MonadMutate m v, MonadFork m, Lattice b, HasValue (FEither v b) a, HasValue b a, HasProps m (FEither v b) a, HasProps m b a) => v (FEither v b) -> v (FEither v b) -> m ()
merge v1' v2' = do
  v1 <- deRef v1'
  v2 <- deRef v2'
  oldCont <- MV.mutate v2 $ \v -> (FEither $ Left v1,v)
  MV.mutate v1 (\v -> updateVal @_ @_ @_ @a (v /\ oldCont)) >>= runProps


class (Monad m) => IndMonadNew m v k where
  new :: (HasEmpty (k b), HasValue b a, HasValue (k b) a) => a -> m (v (k b))

class (Monad m) => IndMonadRead m v k where
  read :: (HasValue b a, HasValue (k b) a) => v (k b) -> m a

class (Monad m) => IndMonadMutate m v k where
  mutate :: (HasValue b a, HasValue (k b) a) => v (k b) -> (a -> (a,s)) -> m s

instance (MonadNew m v, Dereference m v) => IndMonadNew m v (FEither v) where
  new a = MV.new (setVal a empty)

instance (MonadRead m v, Dereference m v) => IndMonadRead m v (FEither v) where
  read p = deRef p >>= (getVal <$>) . MV.read

instance (MonadMutate m v, Dereference m v) => IndMonadMutate m v (FEither v) where
  mutate p f = deRef p >>= \p' -> MV.mutate p' (\v -> let (a',s) = f $ getVal v in (setVal a' v, s))




instance HasValue k a => HasValIn (FEither v k) a where
  getValue (FEither (Right v)) = getVal v
  setValue x (FEither (Right v)) = FEither $ Right $ setVal x v

instance HasProps m k a => HasValIn (FEither v k) (PCollection m a) where
  -- getProps :: k -> PCollection m v k a
  getValue (FEither (Right v)) = getProps v
  setValue x (FEither (Right v)) = FEither $ Right $ setProps x v







--
