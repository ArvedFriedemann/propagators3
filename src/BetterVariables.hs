{-# LANGUAGE NoImplicitPrelude #-}
module BetterVariables where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

class MutateTuple x t r where
  mutateTuple :: x -> (t -> (t, r)) -> (x, r)

instance MutateTuple x t r => MutateTuple (a, x) t r where
  mutateTuple (a, b) f = let (mut, ret) = mutateTuple b f in ((a, mut), ret)

instance MutateTuple (a, x) a r where
  mutateTuple (a, b) f = let (mut, ret) = f a in ((mut, b), ret)

class FindInTuple x t where
  findInTuple :: x -> t

instance FindInTuple (b, c) t => FindInTuple (a, (b, c)) t where
  findInTuple = findInTuple . snd

instance FindInTuple (a, x) a where
  findInTuple = fst

class BuildTuple x t where
  buildTuple :: x -> t -> x

instance {-# OVERLAPPABLE #-} BuildTuple x t => BuildTuple (a, x) t where
  buildTuple (a, b) x = (a, buildTuple b x)

instance {-# OVERLAPPING #-} BuildTuple (a, x) a where
  buildTuple (a, b) x = (x, b)

class EmptyTuple x where
  emptyTuple :: x

instance (BoundedMeetSemiLattice a, BoundedMeetSemiLattice b) => EmptyTuple (a, b) where
  emptyTuple = (top, top)

instance (BoundedMeetSemiLattice a, EmptyTuple (b, c)) => EmptyTuple (a, (b, c)) where
  emptyTuple = (top, emptyTuple) 

class (Monad m, MonadVar m v) => KMonadVar m v b a s where
  read :: v b -> m a
  new :: a -> m (v b)
  mutate :: v b -> (a -> (a,s)) -> m s
  inj :: a -> b -> b
  proj :: b -> a
  mty :: b

instance {-# OVERLAPPING #-} (BoundedMeetSemiLattice a, Lattice a, MonadVar m v) => KMonadVar m v a a s where
  read = MV.read
  new = MV.new
  mutate = MV.mutate
  inj = (/\)
  proj = id
  mty = top

-- TODO: some of these could be rewritten in terms of MutateTuple
instance {-# OVERLAPPABLE #-} (KMonadVar m v a a' s, FindInTuple (x, y) a, BuildTuple (x, y) a', EmptyTuple (x, y), MutateTuple (x,y) a' s) => KMonadVar m v (x, y) a' s where
  read = --(proj <$>) . (findInTuple <$>) . MV.read
    fmap ((proj @m @v @a @a' @s) . findInTuple) . MV.read
  new v = MV.new $ buildTuple emptyTuple v
  mutate ptr f = MV.mutate ptr (`mutateTuple` f)
  inj a tp = buildTuple tp a
  proj = (proj @m @v @a @a' @s) . findInTuple
  mty = emptyTuple

f :: forall m v b a c s. (KMonadVar m v b a s, KMonadVar m v b c s) => v b -> m ()
f ptr = do
   val <- read @_ @_ @_ @a @s ptr
   props <- read @_ @_ @_ @c @s ptr
   return ()
