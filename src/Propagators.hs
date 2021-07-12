{-# LANGUAGE NoImplicitPrelude #-}
module Propagators where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice (Lattice,(/\))
import qualified "lattices" Algebra.Lattice as Lat
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "containers" Data.IntMap.Strict (IntMap, (!), (!?))
import qualified "containers" Data.IntMap.Strict as IntMap
import "mtl" Control.Monad.Reader
import "base" Control.Monad.IO.Class
--import "mtl" Control.Monad.Except
import "base" Data.List
import "base" Control.Monad
import "base" Debug.Trace
import "base" Data.Function
import "base" Data.Bifunctor
import "monad-parallel" Control.Monad.Parallel (MonadFork, forkExec)
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique

import "lens" Control.Lens

import "either" Data.Either.Combinators
import "this" PropagatorTypes


-------------------------------------------------------
--Lens Vars
-------------------------------------------------------


data PropState v =
  PS { scopes :: [Int]}

initPS :: PropState v
initPS = PS {scopes = [0]}

-- type RWST r w s m a
-- is a reader with env r, writer with w, state with s, inner monad m

instance (MonadIO m, MonadFork m) => PropUtil (ReaderT (PropState v) m) where
  getScope = asks (head . scopes)
  getScopePath = asks scopes
  scoped m = do
    u <- hash <$> (liftIO newUnique)
    local (\s -> s{scopes = u : scopes s}) m
  parScoped m = do
    s <- ask
    case scopes s of
      (_ : xs) -> do
        local (\s -> s{scopes = xs}) m
      _ -> error "calling parScoped on Parent!"

type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v, MonadNew m v)

type StdPtr v = (forall a. Eq (v a))

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

deRefRaw :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => PtrType v b -> m (PtrType v b)
deRefRaw p'' = getCurrScpPtr @_ @a p'' >>= deRefRaw'
  where deRefRaw' (P p) = MV.read p >>= \v -> case v of
          (Left _,_) -> return (P p)
          (Right p',_) -> deRefRaw' p'

deRef :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => PtrType v b -> m (v (PtrConts v b))
deRef p = getCurrScpPtr @_ @a p >>= (unpackPtrType <$>) . deRefRaw @_ @a

insertNoReplace :: IntMap.Key -> a -> IntMap a -> (IntMap a, a)
insertNoReplace k v mp = let
  (prev, mp') = IntMap.insertLookupWithKey (\_ _ a -> a) k v mp
  in case prev of
    Nothing -> (mp',v)
    Just x -> (mp',x)

readVarMap :: forall b a m v. (PropUtil m, MonadVar m v, Std m b a) => Var v b -> m (PtrType v b)
readVarMap mp = getScopePath >>= \sp -> readVarMapScope @_ @a sp mp


readVarMapScope :: forall b a m v. (
  PropUtil m,
  MonadVar m v,
  Std m b a) => ScopePath -> Var v b -> m (PtrType v b)
readVarMapScope sp mp = do
  (nptr,eqs) <- readVarMapScope' sp mp
  forM eqs (uncurry (dirEqProp' @_ @a))
  return nptr

--returns (resulting ptr, paths that need upward propagation)
---WARNING: assuming that the directional equalities returned are eventually placed!
readVarMapScope' :: (PropUtil m, MonadVar m v, HasTop a) => ScopePath -> Var v a -> m (PtrType v a, [(PtrType v a, PtrType v a)])
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


getCurrScpPtr :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => PtrType v b -> m (PtrType v b)
getCurrScpPtr p = getScopePath >>= flip (getScpPtr @_ @a) p

getScpPtr :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => ScopePath -> PtrType v b -> m (PtrType v b)
getScpPtr currScp (P p) = do
  (_,(scp,scpmp)) <- MV.read p
  if head currScp == head scp
  then return (P p)
  else readVarMapScope @_ @a currScp scpmp

{-


parScoped $ iff nv (const ContinuousInstance) (\v -> scoped' currScp $ write v nv )
-}

readRef :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => PtrType v b -> m b
readRef p = getCurrScpPtr @_ @a p >>= readRef'
  where readRef' (P p') = do
          (val,(scp,scpmp)) <- MV.read p'
          case val of
            Left v -> return v
            Right p'' -> readRef' p''

new :: forall b a m v. (MonadVar m v, PropUtil m, Std m b a) => m (PtrType v b)
new = MV.new (IntMap.empty) >>= readVarMap @_ @a

newLens :: forall b a m v s. (MonadVar m v, PropUtil m, Std m b a) => Lens' b s -> s -> m (PtrType v b)
newLens l v = do
  n <- new @_ @a
  writeLens @_ @a l n v
  return n

readLens :: forall b a m v s. (MonadVar m v, PropUtil m, Std m b a) => Lens' b s -> PtrType v b -> m s
readLens l = ((^. l) <$>) . readRef @_ @a

writeLens :: forall b a m v s. (MonadVar m v, PropUtil m, Std m b a) => Lens' b s -> PtrType v b -> s -> m ()
writeLens l p v = mutateLens_ @_ @a l p (const v)

mutateLens_ :: forall b a m v s.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) => Lens' b s -> PtrType v b -> (s -> s) -> m ()
mutateLens_ l p f = mutateLens @_ @a l p ((,()) . f)

mutateLens :: forall b a m v s k.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) => Lens' b s -> PtrType v b -> (s -> (s,k)) -> m k
mutateLens l' p f' = getCurrScpPtr @_ @a p >>= \p' -> mutateLens' l' p' f'

mutateLensIn :: forall b a m v s k.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) => ScopePath -> Lens' b s -> PtrType v b -> (s -> (s,k)) -> m k
mutateLensIn sp l' p f' = getScpPtr @b @a sp p >>= \p' -> mutateLens' l' p' f'

mutateLens' :: forall b a m v s k.
  ( MonadVar m v,
    PropUtil m) => Lens' b s -> PtrType v b -> (s -> (s,k)) -> m k
mutateLens' l (P p') f = do
  success <- MV.mutate p' (\val -> case val of
    (Left v,rest) -> ((Left $ over l (fst . f) v,rest),
                Just $ snd . f $ v ^. l)
    (Right _,_) -> (val, Nothing))
  case success of
    Just v -> return v
    Nothing -> mutateLens' l (P p') f

mutateDirect :: (MonadVar m v) => PtrType v b -> (b -> (b,s)) -> m s
mutateDirect (P ptr) f = do
  succeed <- MV.mutate ptr (\ptrc -> case ptrc of
    (Left v, rest) ->
      let (v',r) = f v
      in ((Left v', rest), Left r)
    (Right p, rest) -> ((Right p, rest), Right p))
  case succeed of
    Left r -> return r
    Right p -> mutateDirect p f

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








-------------------------------------------------------
--High level propagators
-------------------------------------------------------


idLens :: Lens' a a
idLens = id

write :: forall b a m v .
  (MonadVar m v,
   PropUtil m,
   Std m b a,
   Lattice a) =>
  PtrType v b -> a -> m ()
write adr' val = getCurrScpPtr @_ @a adr' >>= \adr -> write' adr val

write' :: forall b a m v .
  (MonadVar m v,
   PropUtil m,
   Std m b a,
   Lattice a) =>
  PtrType v b -> a -> m ()
write' adr val = mutateDirect adr (\v -> updateVal @_ @a v $ set value (v ^. value /\ val) v) >>= runProps


addPropagator :: forall b a m v.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) =>
  PtrType v b -> (a -> Instantiated) -> (a -> m ()) -> m ()
addPropagator p pred cont = getCurrScpPtr @b @a p >>= \p' -> addPropagator' p' pred cont

addPropagator' :: forall b a m v.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) =>
  PtrType v b -> (a -> Instantiated) -> (a -> m ()) -> m ()
addPropagator' p pred cont = do
  join $ mutateDirect p $ \v ->  case pred (v ^. value) of
      Failed -> (v, return ())
      Instance -> (v, cont $ v ^. value)
      NoInstance -> (set props (ContRec pred (readLens @_ @a value p >>= cont) : (v ^. props)) v, return ())


--second collection is the succeeding propagators, first is the failed one that needs to be written back
notifyPure :: a -> PCollection m a -> (PCollection m a, PCollection m a)
notifyPure val props = (inst, noInst)
  where (_, noInst, inst) = splitInstantiated props (($ val) . crpred)

updateVal :: forall b a m.
  (HasValue b a,
   HasProps m b a,
   Eq a) =>
   b -> b -> (b, PCollection m a)
updateVal old new
  | (old ^. value :: a) == (new ^. value :: a) = (old, [])
  | otherwise = updateVal' new

updateVal' :: forall b a m.
  (HasValue b a,
   HasProps m b a) =>
   b -> (b, PCollection m a)
updateVal' mt = (set props nosuccprops mt, succprops)
  where --TODO: equality check with old value?
    (nosuccprops, succprops) = notifyPure (mt ^. value) (mt ^. props)

runProps :: (MonadFork m) => PCollection m a -> m ()
runProps = mapM_ (forkExec . crcont)



splitInstantiated :: [a] -> (a -> Instantiated) -> ([a],[a],[a])
splitInstantiated lst f = (filter ((== Failed) . f) lst
                          ,filter ((\x -> x == NoInstance
                                    || x == ContinuousInstance) . f) lst
                          ,filter ((\x -> x == Instance
                                    || x == ContinuousInstance) . f) lst)

iff :: forall b a m v.
  ( MonadVar m v,
    PropUtil m,
    Std m b a) =>
  PtrType v b -> (a -> Instantiated) -> (a -> m ()) -> m ()
iff = addPropagator


-----------------------------------
--Pointer merging
-----------------------------------

merge :: forall b a m v.
  ( MonadVar m v,
    PropUtil m,
    StdPtr v,
    Std m b a,
    Lattice b) => PtrType v b -> PtrType v b -> m ()
merge v1 (P v2) = do
  (P v1') <- deRefRaw @_ @a v1 --not perfect, but better than always merging with the topmost pointer.
  --TODO: get the pointer of the current scope!
  unless (P v1' == P v2) $ do
    oldOrPtr <- MV.mutate v2 $ \val -> case val of
      (Left v, rest) -> ((Right (P v1'),rest), Left v)
      (Right p, rest) -> ((Right p, rest), Right p)
    case oldOrPtr of
      (Left oldCont) -> mutateDirect (P v1') (\v -> updateVal @_ @a oldCont $ v /\ oldCont) >>= runProps
      (Right p) -> merge @b @a (P v1') p


-----------------------------------
--Directional equality
-----------------------------------
dirEqProp' :: forall b a m v.
  ( MonadVar m v,
    MonadFork m,
    PropUtil m,
    Std m b a,
    Lattice a) => PtrType v b -> PtrType v b -> m ()
dirEqProp' p1 p2 = addPropagator' @b @a p1 (const ContinuousInstance) (\v -> write' p2 v)

--
