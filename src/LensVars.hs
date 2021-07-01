{-# LANGUAGE NoImplicitPrelude #-}
module LensVars where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import qualified "lattices" Algebra.Lattice as Lat
import "lens" Control.Lens.Lens
import "lens" Control.Lens.Setter
import "lens" Control.Lens.Getter
import "either" Data.Either.Combinators
import "containers" Data.IntMap.Strict (IntMap, (!), (!?))
import qualified "containers" Data.IntMap.Strict as IntMap
import "mtl" Control.Monad.Reader.Class

class HasScope m where
  getScope :: m Int

instance (MonadReader [Int] m)=> HasScope m where
  getScope = head <$> ask

class HasTop a where
  top :: a

instance (Lat.BoundedMeetSemiLattice a) => HasTop a where
  top = Lat.top

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

type StdPtr v = (forall a. Eq (v a))

type Var v a = v (IntMap (PtrType v a))
type PtrConts v a = (Either a (PtrType v a), (Int, Var v a))
newtype PtrType v a = P (v (PtrConts v a))

instance (forall a. Eq (v a)) => Eq (PtrType v a) where
  (P p1) == (P p2) = p1 == p2

unpackPtrType :: PtrType v a -> v (PtrConts v a)
unpackPtrType (P p) = p

deRefRaw :: (MonadRead m v) => PtrType v a -> m (PtrType v a)
deRefRaw (P p) = MV.read p >>= \v -> case v of
  (Left _,_) -> return (P p)
  (Right p',_) -> deRefRaw p'

deRef :: (MonadRead m v) => PtrType v a -> m (v (PtrConts v a))
deRef = (unpackPtrType <$>) . deRefRaw

insertNoReplace :: IntMap.Key -> a -> IntMap a -> (IntMap a, a)
insertNoReplace k v mp = let
  (prev, mp') = IntMap.insertLookupWithKey (\_ _ a -> a) k v mp
  in case prev of
    Nothing -> (mp',v)
    Just x -> (mp',x)

readVarMap :: (HasScope m, MonadVar m v, HasTop a) => Var v a -> m (PtrType v a)
readVarMap pm = do
  mp <- MV.read pm
  currScp <- getScope
  case mp !? currScp of
    Just v -> return v
    Nothing -> do
      nv <- P <$> MV.new (Left top, (currScp,pm))
      MV.mutate pm (insertNoReplace currScp nv)

readRef :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m a
readRef p = do
  currScp <- getScope
  p' <- deRef p
  (val,(scp,scpmp)) <- MV.read p'
  if currScp == scp
  then case val of
    Left v -> return v
    Right p -> readRef p
  else readVarMap scpmp >>= readRef

new :: (MonadVar m v, HasScope m, HasTop a) => m (PtrType v a)
new = MV.new (IntMap.empty) >>= readVarMap

newLens :: (HasTop a, MonadVar m v, HasScope m) => Lens' a b -> b -> m (PtrType v a)
newLens l v = do
  n <- new
  writeLens l n v
  return n

readLens :: (MonadVar m v, HasScope m, HasTop a) => Lens' a b -> PtrType v a -> m b
readLens l = ((^. l) <$>) . readRef

writeLens :: (MonadMutate m v, MonadRead m v, HasScope m) => Lens' a b -> PtrType v a -> b -> m ()
writeLens l p v = mutateLens_ l p (const v)

mutateLens_ :: (MonadMutate m v, MonadRead m v, HasScope m) => Lens' a b -> PtrType v a -> (b -> b) -> m ()
mutateLens_ l p f = mutateLens l p ((,()) . f)

mutateLens :: (MonadMutate m v, MonadRead m v, HasScope m) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLens l p f = do
  p' <- deRef p
  currScp <- getScope
  success <- MV.mutate p' (\val -> case val of
    (Left v,rest) -> ((Left $ over l (fst . f) v,rest),
                Just $ snd . f $ v ^. l)
    (Right _,_) -> (val, Nothing))
  case success of
    Just v -> return v
    Nothing -> mutateLens l p f





--
