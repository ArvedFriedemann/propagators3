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

import "lens" Control.Lens.Lens
import "lens" Control.Lens.Setter
import "lens" Control.Lens.Getter

import "either" Data.Either.Combinators


import "this" LensVars

class HasValue p a where
  value :: Lens' p a

class HasProps m p a where
  props :: Lens' p (PCollection m a)

idLens :: Lens' a a
idLens = lens id const

write :: forall b a m v .
  (MonadFork m,
   MonadMutate m v,
   HasScope m,
   HasValue b a,
   HasProps m b a,
   Lattice a) =>
  PtrType v b -> a -> m ()
write adr val = mutateLens idLens adr (\v -> updateVal @_ @a $ set value (v ^. value /\ val) v) >>= runProps


addPropagator :: forall b a m v.
  ( MonadVar m v,
    HasScope m,
    HasTop b,
    HasValue b a,
    HasProps m b a) =>
  PtrType v b -> (a -> Instantiated) -> (a -> m ()) -> m ()
addPropagator p pred cont =
  join $ mutateLens idLens p $ \v ->  case pred (v ^. value) of
      Failed -> (v, return ())
      Instance -> (v, cont $ v ^. value)
      NoInstance -> (set props (ContRec pred (readLens value p >>= cont) : (v ^. props)) v, return ())

--second collection is the succeeding propagators, first is the failed one that needs to be written back
notifyPure :: a -> PCollection m a -> (PCollection m a, PCollection m a)
notifyPure val props = (inst, noInst)
  where (_, noInst, inst) = splitInstantiated props (($ val) . crpred)

updateVal :: forall b a m.
  (HasValue b a,
   HasProps m b a) =>
   b -> (b, PCollection m a)
updateVal mt = (set props nosuccprops mt, succprops)
  where --TODO: equality check with old value?
    (nosuccprops, succprops) = notifyPure (mt ^. value) (mt ^. props)

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

iff :: forall b a m v.
  ( MonadVar m v,
    HasScope m,
    HasTop b,
    HasValue b a,
    HasProps m b a) =>
  PtrType v b -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff = addPropagator


-----------------------------------
--Pointer merging
-----------------------------------

merge :: forall b a m v.
  ( MonadFork m,
    MonadMutate m v,
    HasScope m,
    StdPtr v,
    HasValue b a,
    HasProps m b a,
    Lattice b) => PtrType v b -> PtrType v b -> m ()
merge v1 (P v2) = do
  v1' <- deRefRaw v1 --not perfect, but better than always merging with the topmost pointer.
  unless (v1' == P v2) $ do
    oldOrPtr <- MV.mutate v2 $ \val -> case val of
      (Left v, rest) -> ((Right v1',rest), Left v)
      (Right p, rest) -> ((Right p, rest), Right p)
    case oldOrPtr of
      (Left oldCont) -> mutateLens idLens v1' (\v -> updateVal @_ @a $ v /\ oldCont) >>= runProps
      (Right p) -> merge @b @a v1' p



--
