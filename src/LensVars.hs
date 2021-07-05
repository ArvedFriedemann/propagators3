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
import "base" Control.Monad.IO.Class
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique
import "base" Debug.Trace

class HasScope m where
  getScope :: m Int
  scoped :: m a -> m a
  parScoped :: m a -> m a

instance (MonadReader [Int] m, MonadIO m)=> HasScope m where
  getScope = head <$> ask
  scoped m = do
    u <- hash <$> (liftIO newUnique)
    local (u :) m
  parScoped m = do
    s <- ask
    case s of
      (_ : _) -> local tail m
      _ -> error "calling parScoped on Parent!"

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

deRefRaw :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m (PtrType v a)
deRefRaw p'' = getCurrScpPtr p'' >>= deRefRaw'
  where deRefRaw' (P p) = MV.read p >>= \v -> case v of
          (Left _,_) -> return (P p)
          (Right p',_) -> deRefRaw' p'

deRef :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m (v (PtrConts v a))
deRef p = getCurrScpPtr p >>= (unpackPtrType <$>) . deRefRaw

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
      --TODO: Need to create upwards propagators.
      nv <- P <$> MV.new (Left top, (currScp,pm))
      MV.mutate pm (insertNoReplace currScp nv)

getCurrScpPtr :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m (PtrType v a)
getCurrScpPtr (P p) = do
  currScp <- getScope
  (_,(scp,scpmp)) <- MV.read p
  if currScp == scp
  then return (P p)
  else readVarMap scpmp

readRef :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m a
readRef p = getCurrScpPtr p >>= readRef'
  where readRef' (P p') = do
          (val,(scp,scpmp)) <- MV.read p'
          case val of
            Left v -> return v
            Right p'' -> readRef' p''

new :: (MonadVar m v, HasScope m, HasTop a) => m (PtrType v a)
new = MV.new (IntMap.empty) >>= readVarMap

newLens :: (HasTop a, MonadVar m v, HasScope m, Show a) => Lens' a b -> b -> m (PtrType v a)
newLens l v = do
  n <- new
  writeLens l n v
  return n

readLens :: (MonadVar m v, HasScope m, HasTop a) => Lens' a b -> PtrType v a -> m b
readLens l = ((^. l) <$>) . readRef

writeLens :: (MonadVar m v, HasScope m, HasTop a, Show a) => Lens' a b -> PtrType v a -> b -> m ()
writeLens l p v = mutateLens_ l p (const v)

mutateLens_ :: (MonadVar m v, HasScope m, HasTop a, Show a) => Lens' a b -> PtrType v a -> (b -> b) -> m ()
mutateLens_ l p f = mutateLens l p ((,()) . f)

mutateLens :: forall m v a b s.
  ( MonadVar m v,
    HasScope m,
    HasTop a,
    Show a) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLens l' p f' = getCurrScpPtr p >>= \p' -> mutateLens' l' p' f'
  where mutateLens' :: Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
        mutateLens' l (P p') f = do
          success <- MV.mutate p' (\val -> case val of
            (Left v,rest) -> ((Left $ over l (fst . f) v,rest),
                        Just $ snd . f $ v ^. l)
            (Right _,_) -> (val, Nothing))
          case success of
            Just v -> return v
            Nothing -> mutateLens' l (P p') f





--
