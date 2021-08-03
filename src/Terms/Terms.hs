module Terms.Terms where

import "this" Util
import "this" Class.MonadProp
import "this" PropagatorTypes
import "containers" Data.Set (Set, lookupMin)
import qualified "containers" Data.Set as S
import "containers" Data.Map (Map)
import qualified "containers" Data.Map as M
import "base" Control.Monad
import "lattices" Algebra.Lattice (Lattice,BoundedMeetSemiLattice,BoundedJoinSemiLattice,(/\),(\/))
import qualified "lattices" Algebra.Lattice as Lat

data TermConst = CUST String | TBOT
  deriving (Show, Eq, Ord)

data TermSet v a = TS {
  constants :: Set TermConst,
  variables :: Set (TSP v a),
  applications :: Set (TSP v a, TSP v a)
}
deriving instance (Show a, StdPtr v) => Show (TermSet v a)
deriving instance (Eq a, StdPtr v) => Eq (TermSet v a)
deriving instance (Ord a, StdPtr v) => Ord (TermSet v a)

type TSP v a = v (TermSet v a)

--WARNING, TODO: PropBot for terms!

instance (StdPtr v) => Lattice (TermSet v a) where
 (TS c1 v1 a1) /\ (TS c2 v2 a2) = if S.size c > 1
                                  then TS (S.singleton TBOT) v a
                                  else if S.size c > 0 && S.size a > 0
                                  then TS (S.singleton TBOT) v a
                                  else TS c v a
  where (c, v, a) = (S.union c1 c2, S.union v1 v2, S.union a1 a2)
 _ \/ _ = undefined

instance (StdPtr v) => BoundedMeetSemiLattice (TermSet v a) where
  top = emptyTS
instance (StdPtr v) => BoundedJoinSemiLattice (TermSet v a) where
  bottom = cset TBOT

instance HasDecTop (TermSet v a) where
  isTop (TS a b c) = and [S.null a, S.null b, S.null c]

instance HasDecBot (TermSet v a) where
  isBot ts = S.member TBOT $ constants ts

emptyTS :: TermSet v a
emptyTS = TS S.empty S.empty S.empty

aplset :: (TSP v a, TSP v a) -> TermSet v a
aplset t = emptyTS{applications = S.singleton t}

varset :: TSP v a -> TermSet v a
varset t = emptyTS{variables = S.singleton t}

cset :: TermConst -> TermSet v a
cset t = emptyTS{constants = S.singleton t}

newTSP :: (MonadProp m v, StdPtr v, StdLat a) => m (TSP v a)
newTSP = new <<= termListener

eqAll :: (MonadProp m v, StdPtr v, StdLat a) => Set (TSP v a) -> m ()
eqAll (S.toList -> []) = return ()
eqAll (S.toList -> (x : xs)) = forM_ xs (merge x)

termListenerSlow :: (MonadProp m v, StdPtr v, StdLat a) => TSP v a -> m ()
termListenerSlow ptr = readUpdate ptr $ \TS {..} -> do
  eqAll variables
  eqAll (S.map fst applications)
  eqAll (S.map snd applications)
  --TODO: PropBot

eqStream :: (MonadProp m v, StdPtr v, StdLat a) => v a -> (a -> (Set (v a))) -> m ()
eqStream p f = eqStream' p f S.empty

eqStream' :: (MonadProp m v, StdPtr v, StdLat a) => v a -> (a -> (Set (v a))) -> (Set (v a)) -> m ()
eqStream' ptr ptrSet alreadyMerged = iff ptr
  (\ts -> case S.difference (ptrSet ts) alreadyMerged of
            (S.toList -> []) -> NoInstance
            v -> Instance v )
  $ \newvars -> do
    case alreadyMerged of
      (S.toList -> []) -> case newvars of
        (S.toList -> (x : xs)) -> forM_ xs (merge x)
      (S.findMin -> x) -> forM_ (S.toList newvars) (merge x)
    eqStream' ptr ptrSet (S.union alreadyMerged newvars)


termListener :: (MonadProp m v, StdPtr v, StdLat a) => TSP v a -> m ()
termListener ptr = do
  eqStream ptr variables
  eqStream ptr (S.map fst . applications)
  eqStream ptr (S.map snd . applications)
  --TODO: PropBot


---------------------------------------------------
-- Helper functions
---------------------------------------------------

map_var_rep :: (MonadProp m v, StdPtr v, StdLat a) => TSP v a -> (TSP v a -> m ()) -> m ()
map_var_rep ptr f = do
  iffm ptr (lookupMin . variables) f
  iffm ptr (lookupMin . applications) (\ (p1,p2) -> map_var_rep p1 f >> map_var_rep p2 f)
--
split_const_appl_rep :: (MonadProp m v, StdPtr v, StdLat a) =>
  TSP v a -> (TermConst -> m ()) -> ((TSP v a, TSP v a) -> m ()) -> m ()
split_const_appl_rep ptr constcase aplcase = do
  iffm ptr (lookupMin . constants) constcase
  iffm ptr (lookupMin . applications) aplcase

foldnew_const_appl_rep :: (MonadProp m v, StdPtr v, StdLat a) =>
  TSP v a ->
  (TSP v a -> TermConst -> m ()) ->
  (TSP v a -> (TSP v a, TSP v a) -> m ()) ->
  m (TSP v a)
foldnew_const_appl_rep ptr constcase aplcase = do
  nptr <- newTSP
  foldeq_const_appl_rep ptr nptr constcase aplcase
  return nptr

foldeq_const_appl_rep :: (MonadProp m v, StdPtr v, StdLat a) =>
  TSP v a -> TSP v a ->
  (TSP v a -> TermConst -> m ()) ->
  (TSP v a -> (TSP v a, TSP v a) -> m ()) ->
  m ()
foldeq_const_appl_rep ptr eqptr constcase aplcase = split_const_appl_rep ptr
  (constcase eqptr)
  (\(p1,p2) -> do
    (p1',p2') <- (,) <$> newTSP <*> newTSP
    p1rec <- foldeq_const_appl_rep p1 p1' constcase aplcase
    p2rec <- foldeq_const_appl_rep p2 p2' constcase aplcase
    aplcase eqptr (p1', p2')
    )

----------------------------------------------------
--refresh
----------------------------------------------------

refresh :: (MonadProp m v, StdPtr v, StdLat a) =>
  Set TermConst -> TSP v a -> m (TSP v a)
refresh s ptr = do
  mp <- M.fromList <$> (forM (S.toList s) (\c -> (c,) <$> new))
  foldnew_const_appl_rep ptr
    (\ eqptr c -> do
    case M.lookup c mp of
      Just v -> merge v eqptr --TODO: don't create eqptr in this case in the first place
      Nothing -> write eqptr $ cset c)
    (\eqptr (p1,p2) -> write eqptr $ aplset (p1,p2))



--
