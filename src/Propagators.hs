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

class (HasValIn p a) => HasValue p a | p -> a where
  getVal :: p -> a
  getVal = getValue
  setVal :: a -> p -> p
  setVal = setValue

class (HasValIn k (PCollection m v k a)) => HasProps m v k a | k -> a where
  getProps :: k -> PCollection m v k a
  getProps = getValue
  setProps :: PCollection m v k a -> k -> k
  setProps = setValue

class HasEmpty k where
  empty :: k

new :: forall m v k a. (MonadNew m v, HasEmpty k, HasValue k a, HasProps m v k a) => a -> m (v k)
new val = MV.new $ setProps @m @v [] . setVal val $ empty

--read p >>= m ~> watch p (read p >>= m)
read :: (MonadRead m v, HasValue k a) => v k -> m a
read adr = (getVal <$> (MV.read adr))

write :: forall m v k a.
  (MonadFork m, MonadMutate m v, HasValue k a, HasProps m v k a, Eq a, Lattice a) =>
  v k -> a -> m ()
write adr val = MV.mutate adr update >>= mapM_ (forkExec . crcont)
  where
    update :: k -> (k,PCollection m v k a)
    update v = (setProps nosuccprops (setVal mt v), succprops)
      where
        mt :: a
        mt = getVal v /\ val
        (nosuccprops, succprops) =
          if getVal v == mt -- no change
          then (getProps v,[])
          else notify mt (getProps v)

addPropagator :: (MonadMutate m v, HasValue k a, HasProps m v k a) =>
  v k -> (a -> Instantiated) -> (a -> m ()) -> m ()
addPropagator p pred cont =
  join $ MV.mutate p $ \v -> case pred (getVal v) of
      Failed -> (v, return ())
      Instance -> (v, cont $ getVal v)
      NoInstance -> (setProps (ContRec p pred (read p >>= cont) : getProps v) v, return ())

--second collection is the succeeding propagators, first is the failed one that needs to be written back
notify :: a -> PCollection m v k a -> (PCollection m v k a, PCollection m v k a)
notify val props = (inst, noInst)
  where (_, noInst, inst) = splitInstantiated props (($ val) . crpred)


data ContRec m v k a = ContRec {
  crptr :: v k, --might not be needed
  crpred :: a -> Instantiated,
  crcont :: m ()
}



type PCollection m v k a = [ContRec m v k a]

data Instantiated = Failed | NoInstance | Instance
  deriving (Show, Eq, Ord)

splitInstantiated :: [a] -> (a -> Instantiated) -> ([a],[a],[a])
splitInstantiated lst f = (filter ((== Failed) . f) lst
                          ,filter ((== NoInstance) . f) lst
                          ,filter ((== Instance) . f) lst)

iff :: (MonadMutate m v, HasValue k a, HasProps m v k a) =>
  v k -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff p pred m = addPropagator p pred m
