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
  getScopePath :: m ScopePath
  scoped :: m a -> m a
  scoped' :: Int -> m a -> m a
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
type ScopePath = [Int]

type Var v a = v (IntMap (PtrType v a))
type PtrConts v a = (Either a (PtrType v a), (ScopePath, Var v a))
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
readVarMap = (fst <$>) . readVarMap'

--returns (resulting ptr, paths that need upward propagation)
readVarMapScope :: (HasScope m, MonadVar m v, HasTop a) => ScopePath -> Var v a -> m (PtrType v a, [ScopePath])
readVarMapScope currScp pm = do
  mp <- MV.read pm
  case mp !? head currScp of
    Just v -> return v
    Nothing -> do
      nv <- P <$> MV.new (Left top, (currScp,pm))
      MV.mutate pm (\mp ->
        let (mp',nptr) = insertNoReplace currScp nv
        in
        if nptr == nv
        then (mp', (nptr,[]))
        else let {
          getHighestParent [] = Nothing
          getHighestParent (x : xs)
            | x `IntMap.member` mp = Just [x]
            | otherwise = (x :) <$> getHighestParent xs
          highestParent = getHighestParent currScp
        }
        in (np', (nptr, highestParent ++ childpaths))
        )

--returns true if new value was created
readVarMap' :: (HasScope m, MonadVar m v, HasTop a) => Var v a -> m (PtrType v a, Bool)
readVarMap' pm = getScopePath >>= flip readVarMapScope pm


getCurrScpPtr :: (MonadVar m v, HasScope m, HasTop a) => PtrType v a -> m (PtrType v a)
getCurrScpPtr (P p) = do
  currScp <- getScopePath
  (_,(scp,scpmp)) <- MV.read p
  if head currScp == head scp
  then return (P p)
  else do
    (p', isNew) <- readVarMap' scpmp
    if not isNew
    then return p'
    else do
      {-}
      --TODO: upwards propagators
      let (csp, spp, pathToCommonScope) = longestReverseCommonTail currScope scp
      in readVarMap (head pathToCommonScope) scpmp >>= moveScope csp
      where
        moveScope [] l = return l
        moveScope (x : xs) l = readVarMap x scpmp >>= placeProp >>= moveScope xs
        -}

{-


parScoped $ iff nv (const ContinuousInstance) (\v -> scoped' currScp $ write v nv )
-}

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

longestReverseCommonTail :: (Eq a) => [a] -> [a] -> ([a],[a],[a])
longestReverseCommonTail p1 p2 = lct (reverse p1) (reverse p2)
  where lct [] x  = ([],x,[])
        lct x  [] = (x,[],[])
        lct a@(x:xs) b@(y:ys)
          | x != y = (a,b,[])
          | otherwise = let (l, r, t) = lct xs ys
                        in (l, r, x:t)


--
