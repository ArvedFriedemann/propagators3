{-# LANGUAGE NoImplicitPrelude #-}
module Propagators where

import Prelude hiding (read)
import "monad-var" MonadVar.Classes (MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
--import "mtl" Control.Monad.Except
import "base" Data.List
import "monad-parallel" Control.Monad.Parallel (MonadFork, forkExec)


type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v)

class HasValIn p a where
  getValue :: p -> a
  setValue :: p -> a -> p

class (HasValIn p a) => HasValue p a where
  getVal :: p -> a
  getVal = getValue
  setVal :: p -> a -> p
  setVal = setValue

class (HasValIn p (PCollection m v a)) => HasProps p v a m where
  getProps :: p -> PCollection m v a
  getProps = getValue
  setProps :: p -> PCollection m v a -> p
  setProps = setValue


--read p >>= m ~> watch p (read p >>= m)
read :: (MonadRead m v, HasValue k a) => v k -> (a -> m b) -> m b
read adr m = {-TODO: register listener-} (getVal <$> (MV.read adr)) >>= m

write :: forall m v k a.
  (MonadFork m, MonadMutate m v, HasValue k a, HasProps k v a m, Eq a, Lattice a) =>
  v k -> a -> m ()
-- TODO: notify returns a set of propagators that haven't fired. Use them.
write adr val = MV.mutate adr update >>= mapM_ (forkExec . crcont) --TODO: perform the propagators concurrently
  where
    update :: k -> (k,PCollection m v a)
    update v = (setProps (setVal v mt) nosuccprops, succprops)
      where
        mt :: a
        mt = getVal v /\ val
        (nosuccprops, succprops) =
          if getVal v == mt -- no change
          then (getProps v,[])
          else notify mt (getProps v)

--second collection is the succeeding propagators, first is the failed one that needs to be written back
notify :: a -> PCollection m v a -> (PCollection m v a, PCollection m v a)
notify val props = partition (not . ($ val) . crpred) props


data ContRec m v a = ContRec {
  crptr :: v a, --might not be needed
  crpred :: (a -> Bool),
  crcont :: m ()
}

type PCollection m v a = [ContRec m v a]

iff :: (MonadRead m v, HasValue a a) =>
  v a -> (a -> Bool) -> (a -> m ()) -> m ()
iff p pred m = read p $ \p' ->
  if pred p' then m p' else undefined --TODO: throwError (ContRec p pred (read p >>= m))

{-
blah = do
  iff x prop1 $ do
    iff y prop2 $ do
      -- do something with them
-}
