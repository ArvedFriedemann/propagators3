module Terms.Terms where

import "this" Class.MonadProp
import "this" PropagatorTypes
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
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
  top = TS S.empty S.empty S.empty
instance (StdPtr v) => BoundedJoinSemiLattice (TermSet v a) where
  bottom = TS (S.singleton TBOT) S.empty S.empty

eqAll :: (MonadProp m v, StdPtr v, StdLat a) => Set (TSP v a) -> m ()
eqAll (S.toList -> []) = return ()
eqAll (S.toList -> (x : xs)) = forM_ xs (merge x)

termListenerSlow :: (MonadProp m v, StdPtr v, StdLat a) => TSP v a -> m ()
termListenerSlow ptr = readUpdate ptr $ \TS {..} -> do
  eqAll variables
  eqAll (S.map fst applications)
  eqAll (S.map snd applications)

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



--
