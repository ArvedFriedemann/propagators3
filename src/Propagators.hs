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


import "this" LensVars

class HasValue p a where
  value :: Lens' p a

class HasProps p where
  props :: Lens' p (PCollection m a)

class HasEmpty k where
  empty :: k

idLens :: Lens' a a
idLens = lens id const

write ::
  (MonadFork m, HasValue b a) =>
  PtrType v b -> a -> m ()
write adr val = mutateLens idLens adr (\v -> updateVal $ set value (v ^. value /\ val) v) >>= runProps


addPropagator :: (HasValue b a, HasProps b) =>
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

updateVal :: (HasValue b a, HasProps b)
  => a -> (a, PCollection m a)
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

iff :: () =>
  PtrType v a -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff p pred m = addPropagator p pred m


-----------------------------------
--Pointer merging
-----------------------------------

merge :: (MonadMutate m v) => PtrType v a -> PtrType v a -> m ()
merge v1' v2' = do
  v1 <- deRefShallow v1'
  v2 <- deRefShallow v2'
  (fromRight -> oldCont) <- MV.mutate (unpackPtrType v2) $ \v -> (Right v1,v)
  --There is a problem here: When the reference is not within the pointer directly, transfer is not threadsafe. Propagators can still change the old value, as it is not invalidated!
  MV.mutate v1 (\v -> updateVal $ v /\ oldCont) >>= runProps



--
