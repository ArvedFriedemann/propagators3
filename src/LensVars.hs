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
import "mtl" Control.Monad.State
import "base" Control.Monad.IO.Class
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique
import "base" Debug.Trace
import "base" Data.List
import "base" Data.Function
import "base" Control.Monad

class PropUtil m v | m -> v where
  getScope :: m Int
  getScopePath :: m ScopePath
  scoped :: m a -> m a
  scoped' :: Int -> m a -> m a
  parScoped :: m a -> m a
  pushAction :: Action v -> m ()
  flushActions :: m [Action v]

data Action v = forall a. Action (PtrType v a, PtrType v a)

class ActionBuffer m v where


data PropState v =
  PS { scopes :: [Int], actions :: [Action v] }

-- type RWST r w s m a
-- is a reader with env r, writer with w, state with s, inner monad m

instance (MonadIO m) => PropUtil (StateT (PropState v) m) v where
  getScope = gets (head . scopes)
  getScopePath = gets scopes
  scoped m = do
    u <- hash <$> (liftIO newUnique)
    s <- get
    modify (\s -> s{scopes = u : scopes s})
    r <- m
    put s
    return r
  parScoped m = do
    s <- get
    case scopes s of
      (_ : xs) -> do
        modify (\s -> s{scopes = xs})
        r <- m
        put s
        return r
      _ -> error "calling parScoped on Parent!"
  pushAction a = modify (\s -> s{actions = a : actions s})
  flushActions = do
    r <- gets actions
    modify (\s -> s{actions = []})
    return r

class HasTop a where
  top :: a

instance (Lat.BoundedMeetSemiLattice a) => HasTop a where
  top = Lat.top

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

type StdPtr v = (forall a. Eq (v a))
type ScopePath = [Int]

data ScopedPtr v a = ScopedPtr {
  s_ptr :: PtrType v a,
  s_scp :: ScopePath
}

type Var v a = v (IntMap (ScopedPtr v a, [ScopedPtr v a]))
type PtrConts v a = (Either a (PtrType v a), (ScopePath, Var v a))
newtype PtrType v a = P (v (PtrConts v a))

instance (forall a. Eq (v a)) => Eq (PtrType v a) where
  (P p1) == (P p2) = p1 == p2

unpackPtrType :: PtrType v a -> v (PtrConts v a)
unpackPtrType (P p) = p

deRefRaw :: (MonadVar m v, PropUtil m v, HasTop a) => PtrType v a -> m (PtrType v a)
deRefRaw p'' = getCurrScpPtr p'' >>= deRefRaw'
  where deRefRaw' (P p) = MV.read p >>= \v -> case v of
          (Left _,_) -> return (P p)
          (Right p',_) -> deRefRaw' p'

deRef :: (MonadVar m v, PropUtil m v, HasTop a) => PtrType v a -> m (v (PtrConts v a))
deRef p = getCurrScpPtr p >>= (unpackPtrType <$>) . deRefRaw

insertNoReplace :: IntMap.Key -> a -> IntMap a -> (IntMap a, a)
insertNoReplace k v mp = let
  (prev, mp') = IntMap.insertLookupWithKey (\_ _ a -> a) k v mp
  in case prev of
    Nothing -> (mp',v)
    Just x -> (mp',x)

readVarMap :: (PropUtil m v, MonadVar m v, HasTop a) => Var v a -> m (PtrType v a)
readVarMap mp = getScopePath >>= \sp -> readVarMapScope sp mp


readVarMapScope :: (PropUtil m v, MonadVar m v, HasTop a) => ScopePath -> Var v a -> m (PtrType v a)
readVarMapScope sp mp = do
  (nptr,eqs) <- readVarMapScope' sp mp
  forM eqs (pushAction . Action)
  return nptr

--returns (resulting ptr, paths that need upward propagation)
---WARNING: assuming that the directional equalities returned are eventually placed!
readVarMapScope' :: (PropUtil m v, MonadVar m v, HasTop a) => ScopePath -> Var v a -> m (PtrType v a, [(PtrType v a, PtrType v a)])
readVarMapScope' currScp pm = do
  mp <- MV.read pm
  case mp !? head currScp of
    Just (s_ptr . fst -> v) -> return (v,[])
    Nothing -> do
      nv <- P <$> MV.new (Left top, (currScp,pm))
      MV.mutate pm (\mp ->
        let hasPtr = IntMap.member (head currScp) mp
        in if hasPtr
        then (mp, (s_ptr $ fst $ mp ! (head currScp), []))
        else let
          rawParents = flip map (IntMap.toList mp) (\(i,(mptr,_)) ->
            (i,longestReverseCommonTail currScp (s_scp mptr)))
          parents = filter (\(_,(_, ptrPart, _)) -> null ptrPart) rawParents
          highestParentScp = fst <$>
            if null parents
            then Nothing -- no parent
            else Just $ maximumBy (compare `on` (\(_,(_,_,tl))-> length tl)) parents
          (mp',nextEqualities) = case highestParentScp of
            Just hpc -> let
                          (hpcPtr, hpcChildren) = mp ! hpc
                          (currScpChildren,nonchildren) = partition (\c -> currScp `isParentOf` (s_scp c)) hpcChildren
                        in --two map adjusts (correcting the old parent, adding the new pointer)
                          (IntMap.insert (head currScp) (ScopedPtr nv currScp, currScpChildren)
                            (IntMap.adjust (\(v,clds) -> (v,(ScopedPtr nv currScp) : nonchildren)) hpc mp),
                          --giving the list of equalities
                          (s_ptr hpcPtr,nv) : (((nv,) . s_ptr) <$> currScpChildren))
            Nothing -> let
                          children = fst <$> flip filter (IntMap.elems mp) (\(sp,_) -> currScp `isParentOf` (s_scp sp))
                       in (IntMap.insert (head currScp) (ScopedPtr nv currScp, children) mp,
                          ((nv,) . s_ptr) <$> children)
             in (mp',(nv,nextEqualities))
        )


getCurrScpPtr :: (MonadVar m v, PropUtil m v, HasTop a) => PtrType v a -> m (PtrType v a)
getCurrScpPtr p = getScopePath >>= flip getScpPtr p

getScpPtr :: (MonadVar m v, PropUtil m v, HasTop a) => ScopePath -> PtrType v a -> m (PtrType v a)
getScpPtr currScp (P p) = do
  (_,(scp,scpmp)) <- MV.read p
  if head currScp == head scp
  then return (P p)
  else readVarMapScope currScp scpmp

{-


parScoped $ iff nv (const ContinuousInstance) (\v -> scoped' currScp $ write v nv )
-}

readRef :: (MonadVar m v, PropUtil m v, HasTop a) => PtrType v a -> m a
readRef p = getCurrScpPtr p >>= readRef'
  where readRef' (P p') = do
          (val,(scp,scpmp)) <- MV.read p'
          case val of
            Left v -> return v
            Right p'' -> readRef' p''

new :: (MonadVar m v, PropUtil m v, HasTop a) => m (PtrType v a)
new = MV.new (IntMap.empty) >>= readVarMap

newLens :: (HasTop a, MonadVar m v, PropUtil m v, Show a) => Lens' a b -> b -> m (PtrType v a)
newLens l v = do
  n <- new
  writeLens l n v
  return n

readLens :: (MonadVar m v, PropUtil m v, HasTop a) => Lens' a b -> PtrType v a -> m b
readLens l = ((^. l) <$>) . readRef

writeLens :: (MonadVar m v, PropUtil m v, HasTop a, Show a) => Lens' a b -> PtrType v a -> b -> m ()
writeLens l p v = mutateLens_ l p (const v)

mutateLens_ :: (MonadVar m v, PropUtil m v, HasTop a, Show a) => Lens' a b -> PtrType v a -> (b -> b) -> m ()
mutateLens_ l p f = mutateLens l p ((,()) . f)

mutateLens :: forall m v a b s.
  ( MonadVar m v,
    PropUtil m v,
    HasTop a,
    Show a) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLens l' p f' = getCurrScpPtr p >>= \p' -> mutateLens' l' p' f'

mutateLensIn :: forall m v a b s.
  ( MonadVar m v,
    PropUtil m v,
    HasTop a,
    Show a) => ScopePath -> Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
mutateLensIn sp l' p f' = getScpPtr sp p >>= \p' -> mutateLens' l' p' f'

mutateLens' :: forall m v a b s.
  ( MonadVar m v,
    PropUtil m v,
    HasTop a,
    Show a) => Lens' a b -> PtrType v a -> (b -> (b,s)) -> m s
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
          | x /= y = (a,b,[])
          | otherwise = let (l, r, t) = lct xs ys
                        in (l, r, x:t)

isParentOf :: (Eq a) => [a] -> [a] -> Bool
isParentOf p1 p2 = null p1part
  where (p1part,_,_) = longestReverseCommonTail p1 p2

--
