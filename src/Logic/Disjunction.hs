module Logic.Disjunction where

import "this" Util
import "this" Class.MonadProp
import "this" PropagatorTypes hiding (scoped, parScoped)
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "base" Control.Monad

class Promoter m v a where
  promoteAction :: v a -> m ()

disjunctForkPromote :: forall a v m.
  (MonadProp m v, StdLat a, HasDecBot a, Promoter m v a) =>
  v a -> [m ()] -> m ()
disjunctForkPromote p lst = disjunctFork p (zip (repeat $ promoteAction p) lst)

disjunctForkPromote' :: forall a v m.
  (MonadProp m v, StdLat a, HasDecBot a) =>
  v a -> [m ()] -> m ()
disjunctForkPromote' p lst = disjunctFork p (zip (repeat $ promote p) lst)

disjunctFork :: forall a v m. (MonadProp m v, StdLat a, HasDecBot a) =>
  v a -> [(m (), m ())] -> m ()
disjunctFork p = disjunctFork' p isBot

disjunctFork' :: forall a v m. (MonadProp m v, StdLat a) =>
  v a -> (a -> Bool) -> [(m (), m ())] -> m ()
disjunctFork' obvar outrulecrit branches = do
  outruled <- new :: m (v (RevSet Int))
  let indexed = zip [0..] branches
  let len = length indexed
  forM_ indexed $ \(i, (b,promote)) -> do
    scoped @_ @v $ do
      b
      iffb obvar outrulecrit $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
      iffb outruled (\(RS s) -> (S.size s == (len - 1)) && (not $ S.member i s)) promote
