module Terms.Terms where

import "this" Class.MonadProp
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S

data TermConst = CUST String
  deriving (Show, Eq, Ord)

data TermSet v a = TS {
  constants :: Set TermConst,
  variables :: Set (TSP v a),
  applications :: Set (TSP v a, TSP v a)
}

type TSP v a = v (TermSet v a)

--type StdPtr v = (forall a. Show (v a), forall a. Eq (v a), forall a. Ord (v a))

{-}
--TODO: Lattice for TermSet

eqAll :: (MonadProp m v) => Set (TSP v a) -> m ()
eqAll (S.toList -> []) = return ()
eqAll (S.toList -> (x : xs)) = forM_ xs (merge x)

termListenerSlow :: (MonadProp m v) => TSP v a -> m ()
termListenerSlow ptr = readUpdate ptr $ \TS {..} -> do
  eqAll variables
  eqAll (map left applications)
  eqAll (map right applications)

eqStream' :: v p -> (p -> (Set (TSP v a))) -> (Set (TSP v a)) -> m ()
eqStream' ptr ptrSet alreadyMerged = iff ptr $
  \ts -> case S.difference (ptrSet ts) alreadyMerged of
            (toList -> []) -> NoInstance
            _ -> Instance
-}







--
