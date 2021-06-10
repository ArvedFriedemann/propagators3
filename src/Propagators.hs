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

class StackedPointer m kp k | kp -> k, kp -> m where
  deRef :: kp -> m k
  mutate :: kp -> (k -> (k,a)) -> m a
  mutate_ :: kp -> (k -> k) -> m ()
  mutate_ k f = mutate k (\x -> (f x, ()))
  createSP :: k -> m kp

new :: forall m v kp k a.
  (MonadNew m v, HasEmpty k, HasValue k a, HasProps m v k a,
  StackedPointer m (v kp) k) => a -> m (v kp)
new val = createSP $ setProps @m @v [] . setVal val $ empty

--read p >>= m ~> watch p (read p >>= m)
read :: (MonadRead m v, HasValue k a, StackedPointer m (v kp) k) => v kp -> m a
read adr = getVal <$> deRef adr

write :: forall m v kp k a.
  (MonadFork m, HasValue k a, HasProps m v k a, StackedPointer m (v kp) k, Eq a, Lattice a) =>
  v kp -> a -> m ()
write adr val = mutate adr update >>= mapM_ (forkExec . crcont)
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

addPropagator :: forall m v kp k a. (Monad m, MonadRead m v, MonadWrite m v, HasProps m v kp a, HasValue k a, StackedPointer m (v kp) k) =>
  v kp -> (a -> Instantiated) -> (a -> m ()) -> m ()
-- TODO: lose MonadRead and MonadWrite via some form of mutation on kp (but it was hard)
addPropagator p pred cont = do
    rp <- MV.read p
    a <- deRef p
    let (r, m) = blah rp (getVal a)
    MV.write p r
    m
  where 
    blah :: kp -> a -> (kp, m ())
    blah v a = case pred a of
      Failed -> (v, return ())
      Instance -> (v, cont a)
      NoInstance -> (setProps (ContRec p pred (deRef p >>= cont . getVal) : getProps v) v, return ())

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

iff :: (HasProps m v kp a, StackedPointer m (v kp) k) =>
  v kp -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff = addPropagator
