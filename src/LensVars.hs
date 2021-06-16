{-# LANGUAGE NoImplicitPrelude #-}
module LensVars where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "lens" Control.Lens.Lens
import "lens" Control.Lens.Setter
import "lens" Control.Lens.Getter
import "either" Data.Either.Combinators

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

type StdPtr v = (forall a. Eq (v a))

newtype PtrType v a = P (v (Either a (PtrType v a)))

instance (forall a. Eq (v a)) => Eq (PtrType v a) where
  (P p1) == (P p2) = p1 == p2

unpackPtrType :: PtrType v a -> v (Either a (PtrType v a))
unpackPtrType (P p) = p

deRefRaw :: (MonadRead m v) => PtrType v a -> m (PtrType v a)
deRefRaw (P p) = MV.read p >>= \v -> case v of
  Left _ -> return (P p)
  Right p' -> deRefRaw p'

deRef :: (MonadRead m v) => PtrType v a -> m (v (Either a (PtrType v a)))
deRef = (unpackPtrType <$>) . deRefRaw

readRef :: (MonadRead m v) => PtrType v a -> m a
readRef p = deRef p >>= (fromLeft' <$>) . MV.read

newLens :: (BoundedMeetSemiLattice a, MonadNew m v) => Lens' a b -> b -> m (PtrType v a)
newLens l v = P <$> MV.new (Left $ set l v top)

readLens :: (MonadRead m v) => Lens' a b -> PtrType v a -> m b
readLens l = ((^. l) <$>) . readRef

writeLens :: (MonadMutate_ m v, MonadRead m v) => Lens' a b -> PtrType v a -> b -> m ()
writeLens l p v = deRef p >>= flip MV.mutate_ (mapLeft $ set l v)

mutateLens_ :: (MonadMutate_ m v, MonadRead m v) => Lens' a b -> PtrType v a -> (b -> b) -> m ()
mutateLens_ l p f = deRef p >>= flip MV.mutate_ (mapLeft $ over l f)

mutateLens :: (MonadMutate m v, MonadRead m v) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLens l p f = deRef p >>= flip MV.mutate (\v -> (mapLeft (over l (fst . f)) v, snd . f $ (fromLeft' v) ^. l))





--
