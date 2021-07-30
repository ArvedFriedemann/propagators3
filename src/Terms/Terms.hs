module Terms.Terms where

import "this" Class.MonadProp
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S

data TermConst = CUST String
  deriving (Show, Eq, Ord)

data TermSet v a = TS {
  constants :: Set TermConst
  variables :: Set (TSP v a)
  applications :: Set (TSP v a, TSP v a)
}

type TSP v a = v (TermSet v a)

type StdPtr v = (forall a. Show (v a), Eq (v a), Ord (v a))

--TODO: Lattice for TermSet

eqAll :: Set (TSP v a) -> m ()
eqAll (toList -> []) = return ()
eqAll (toList -> (x : xs)) = forM_ xs (merge x)

termListenerSlow :: (MonadProp m v) => TSP v a -> m ()
termListenerSlow ptr = readUpdate ptr $ \TS {..} -> do
  eqAll variables
  eqAll (map left applications)
  eqAll (map right applications)
