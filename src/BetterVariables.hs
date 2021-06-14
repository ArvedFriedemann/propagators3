{-# LANGUAGE NoImplicitPrelude #-}
module BetterVariables where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

class (Monad m, MonadVar m v) => KMonadVar m v b a where
  read :: v b -> m a
  new :: a -> m (v b)
  mutate :: v b -> (a -> (a,s)) -> m s
  inj :: a -> b -> b
  proj :: b -> a
  mty :: b


instance {-# OVERLAPPABLE #-} (BoundedMeetSemiLattice a, Lattice a, MonadVar m v) => KMonadVar m v a a where
  read = MV.read
  new = MV.new
  mutate = MV.mutate
  inj = (/\)
  proj = id
  mty = top
{-}
class FindInTuple a b t where
  findInTuple :: (a, b) -> t

instance FindInTuple b c t => FindInTuple (a, (b, c)) t where
  findInTuple = snd . findInTuple

instance FindInTuple (a, x) a where
  findInTuple = fst
-}
instance {-# OVERLAPPING #-} (KMonadVar m v a a') => KMonadVar m v (a,b) a' where
  read =  (proj <$>) . (fst <$>) . MV.read
  new v = MV.new (inj v mty, mty)
  mutate ptr f = MV.mutate ptr (\(x,y) -> ((fst $ f x, y),snd $ f x))
  inj a (a',b) = (inj a a', b)
  proj = (proj) . fst
  mty = (mty,mty)

instance {-# OVERLAPPABLE #-} (KMonadVar m v b b') => KMonadVar m v (a,b) b' where
  read = (proj <$>) . (snd <$>) . MV.read
  new v = MV.new (mty, inj v mty)
  mutate ptr f = MV.mutate ptr (\(x,y) -> ((x, fst $ f y),snd $ f y))
  inj b (a,b') = (a, inj b b')
  proj = (proj) . snd
  mty = (mty,mty)

f :: forall m v b a c p1 p2. (KMonadVar p1 m v b a, KMonadVar p2 m v b c) => v b -> m ()
f ptr = do
   val <- read @_ @_ @_ @_ @a ptr
   props <- read @_ @_ @_ @_ @c ptr
   return ()











   --
