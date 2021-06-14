{-# LANGUAGE NoImplicitPrelude #-}
module BetterVariables where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

class KMonadVar p m v b a where
  read :: v b -> m a
  new :: a -> m (v b)
  mutate :: v b -> (a -> (a,s)) -> m s
  inj :: a -> b -> b
  proj :: b -> a
  mty :: b


data MonadVarIDInstance = MonadVarIDInstance
instance (BoundedMeetSemiLattice a, Lattice a, MonadVar m v) => KMonadVar MonadVarIDInstance m v a a where
  read = MV.read
  new = MV.new
  mutate = MV.mutate
  inj = (/\)
  proj = id
  mty = top

data MonadVarLeftInstance a b = MonadVarLeftInstance a b
instance (KMonadVar p1 m v a a', KMonadVar p2 m v b b') => KMonadVar (MonadVarLeftInstance p1 p2) m v (a,b) a' where
  read =  (proj <$>) . (fst <$>) . MV.read
  new v = MV.new (inj v mty)
  mutate ptr f = MV.mutate ptr (\(x,y) -> ((fst $ f x, y),snd $ f x))
  inj a (a',b) = (inj a a', b)
  proj = fst
  mty = (mty,mty)

data MonadVarRightInstance a b = MonadVarRightInstance a b
instance (KMonadVar p1 m v a a', KMonadVar p2 m v b b') => KMonadVar (MonadVarRightInstance p1 p2) m v (a,b) b' where
  read = (proj <$>) . (snd <$>) . MV.read
  new v = MV.new (inj v mty)
  mutate ptr f = MV.mutate ptr (\(x,y) -> ((x, fst $ f y),snd $ f y))
  inj b (a,b') = (a, inj b b')
  proj = snd
  mty = (mty,mty)

f :: forall m v b a c p1 p2. (KMonadVar p1 m v b a, KMonadVar p2 m v b c) => v b -> m ()
f ptr = do
   val <- read @a ptr
   props <- read @c ptr
   return ()











   --
