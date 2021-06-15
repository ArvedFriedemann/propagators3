{-# LANGUAGE NoImplicitPrelude #-}
module LensVars where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "lens" Control.Lens.Lens
import "lens" Control.Lens.Setter
import "lens" Control.Lens.Getter

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

newtype PtrType v a = P (v (Either (v a) (PtrType v a)))
unpackPtrType :: PtrType v a -> v (Either (v a) (PtrType v a))
unpackPtrType (P p) = p

deRef :: (MonadRead m v) => PtrType v a -> m (v a)
deRef (P p) = MV.read p >>= \v -> case v of
  Left p' -> return p'
  Right p' -> deRef p'

deRefShallow :: (MonadRead m v) => PtrType v a -> m (PtrType v a)
deRefShallow (P p) = MV.read p >>= \v -> case v of
  Left _ -> return (P p)
  Right p' -> deRefShallow p'

readRef :: (MonadRead m v) => PtrType v a -> m a
readRef p = deRef p >>= MV.read

newLens :: (BoundedMeetSemiLattice a, MonadNew m v) => Lens' a b -> b -> m (PtrType v a)
newLens l v = MV.new (set l v top) >>= \p -> P <$> MV.new (Left p)

readLens :: (MonadRead m v) => Lens' a b -> PtrType v a -> m b
readLens l = ((^. l) <$>) . readRef

writeLens :: (MonadMutate_ m v, MonadRead m v) => Lens' a b -> PtrType v a -> b -> m ()
writeLens l p v = deRef p >>= flip MV.mutate_ (set l v)

mutateLens_ :: (MonadMutate_ m v, MonadRead m v) => Lens' a b -> PtrType v a -> (b -> b) -> m ()
mutateLens_ l p f = deRef p >>= flip MV.mutate_ (over l f)

mutateLens :: (MonadMutate m v, MonadRead m v) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLens l p f = deRef p >>= flip MV.mutate (\v -> (over l (fst . f) $ v, snd . f $ v ^. l))





--
