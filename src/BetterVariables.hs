{-# LANGUAGE NoImplicitPrelude #-}
module BetterVariables where
{-}
import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

class MutateTuple x t where
  mutateTuple :: x -> (t -> (t, r)) -> (x, r)

instance MutateTuple x t => MutateTuple (a, x) t where
  mutateTuple (a, b) f = let (mut, ret) = mutateTuple b f in ((a, mut), ret)

instance MutateTuple (a, x) a where
  mutateTuple (a, b) f = let (mut, ret) = f a in ((mut, b), ret)

class FindInTuple x t where
  findInTuple :: x -> t

type family If (b :: Bool) (t :: *) (f :: *) :: * where
  If 'True t _ = t
  If 'False _ t = t

type family Or (b :: Bool) (b' :: Bool) :: Bool where
  Or 'False 'False = 'False
  Or _ _ = 'True

type family Contains (t :: *) (struct :: *) :: Bool where
  Contains t (a, b) = Or (Contains t a) (Contains t b)
  Contains t t = 'True
  Contains t b = 'False

instance FindInTuple' x t (Contains t a) =>
  FindInTuple (a, b) t where
  findInTuple = findInTuple'

class FindInTuple' x t (b :: Bool) where
  findInTuple' :: x -> t

instance FindInTuple a t l => FindInTuple' (a, b) t 'True where
  findInTuple' = findInTuple . fst

instance FindInTuple b t l => FindInTuple' (a, b) t 'False where
  findInTuple' = findInTuple . snd

instance FindInTuple (a, x) a where
  findInTuple = fst

instance FindInTuple (a, x) x where
  findInTuple = snd

instance FindInTuple a a where
  findInTuple = id

found :: Bool
found = findInTuple (((), (9 :: Int)), ((6 :: Int), False))

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

class (Monad m, MonadVar m v) => KMonadVar m v b a where
  read :: v b -> m a
  new :: a -> m (v b)
  mutate :: v b -> (a -> (a,s)) -> m s
  inj :: a -> b -> b
  proj :: b -> a
  mty :: b

instance {-# OVERLAPPING #-} (BoundedMeetSemiLattice a, Lattice a, MonadVar m v) => KMonadVar m v a a where
  read = MV.read
  new = MV.new
  mutate = MV.mutate
  inj = (/\)
  proj = id
  mty = top

-- TODO: some of these could be rewritten in terms of MutateTuple
instance {-# OVERLAPPABLE #-} (KMonadVar m v a a', FindInTuple (x, y) a, BuildTuple (x, y) a', EmptyTuple (x, y), MutateTuple (x,y) a') => KMonadVar m v (x, y) a' where
  read = --(proj <$>) . (findInTuple <$>) . MV.read
    fmap ((proj @m @v @a) . findInTuple) . MV.read
  new v = MV.new $ buildTuple emptyTuple v
  mutate ptr f = MV.mutate ptr (`mutateTuple` f)
  inj a tp = buildTuple tp a
  proj = (proj @m @v @a) . findInTuple
  mty = emptyTuple

f :: forall m v b a c. (KMonadVar m v b a, KMonadVar m v b c) => v b -> m ()
f ptr = do
   val <- read @_ @_ @_ @a ptr
   props <- read @_ @_ @_ @c ptr
   return ()
-}
