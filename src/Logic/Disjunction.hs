module Logic.Disjunction where

import "this" Util
import "this" Class.MonadProp
import "this" PropagatorTypes hiding (new, scoped, parScoped)
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "base" Control.Monad
import "base" Debug.Trace

class Promoter m v a where
  promoteAction :: v a -> m ()

disjunctForkPromote :: forall a v m.
  (MonadProp m v, StdLat a, HasDecBot a, Promoter m v a) =>
  v a -> [m ()] -> m ()
disjunctForkPromote p lst = disjunctFork p (zip lst (repeat $ promoteAction p))

disjunctForkPromote' :: forall a v m.
  (MonadProp m v, StdLat a, HasDecBot a) =>
  v a -> [m ()] -> m ()
disjunctForkPromote' p lst = disjunctFork p (zip lst (repeat $ promote p))

disjunctForkPromotePred :: forall a v m.
  (MonadProp m v, StdLat a, HasDecBot a, Promoter m v a) =>
  v a -> (a -> Bool) -> [m ()] -> m ()
disjunctForkPromotePred p pred lst = disjunctFork' p pred (zip lst (repeat $ promoteAction p)) (write p bot)

disjunctForkPromotePred' :: forall a v m.
  (MonadProp m v, StdLat a, HasBot a) =>
  v a -> (a -> Bool) -> [m ()] -> m ()
disjunctForkPromotePred' p pred lst = disjunctFork' p pred (zip lst (repeat $ promote p)) (write p bot)

disjunctForkPromotePredFin :: forall a v m.
  (MonadProp m v, StdLat a) =>
  v a -> (a -> Bool) -> [m ()] -> m () -> m ()
disjunctForkPromotePredFin p pred lst fin = disjunctFork' p pred (zip lst (repeat $ promote p)) fin

disjunctFork :: forall a v m. (MonadProp m v, StdLat a, HasDecBot a) =>
  v a -> [(m (), m ())] -> m ()
disjunctFork p bs = disjunctFork' p isBot bs (write p bot)

disjunctFork' :: forall a v m. (MonadProp m v, StdLat a) =>
  v a -> (a -> Bool) -> [(m (), m ())] -> m () -> m ()
disjunctFork' obvar outrulecrit branches failure  = do
  let indexed = zip [0..] branches
  let len = length indexed
  outruled <- new @_ @v -- :: m (v (RevSet Int))
  --write outruled $ RS $ S.singleton (-1)
  --readUpdate outruled (traceM . ("outruled: "++) . show)
  iffb outruled (\(RS s) -> S.size s >= len) failure
  forM_ indexed $ \(i, (b,promote)) -> do
    scoped @_ @v $ do
      --traceM $ "exec scope "++show i
      b -- >>= \b' -> traceM $ "exec scope "++show i++show b'
      --readUpdate obvar (traceM . (("obvar in "++show i++": ") ++) . show)
      iffb obvar outrulecrit $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
      iffb outruled (\(RS s) -> (S.size s == (len - 1)) && (not $ S.member i s)) $
        (traceM $ "chose "++show i) >> promote
