module RunPropagatorInterface where

import "this" Class.MonadProp
import "this" Propagators hiding (new, write, scoped, parScoped)
import "this" CustomVars
import "this" PropagatorTypes hiding (scoped, parScoped)
import "this" Logic.Disjunction
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader
import "base" Debug.Trace
import "base" Control.Monad.IO.Class
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S

testDisj :: forall v. (v ~ UP) => IO ()
testDisj = runPropM @v $ do
  v1 <- new' []
  disjunctForkPromotePred' v1 (\c -> c /= ["a"]) [
      traceM "scope0" >> readUpdate v1 (lift . putStrLn . (++" in scope0") . show) >>
        write v1 ["a"] >> traceM "writing in scope0",
      traceM "scope1" >> readUpdate v1 (lift . putStrLn . (++" in scope1") . show) >>
        write v1 ["b"] >> traceM "writing in scope1"
    ]
  readUpdate v1 (lift . putStrLn . (++" in orig") . show)

testDisj' :: forall m v. (MonadIO m, MonadProp m v) => m ()
testDisj' = do
  v1 <- new' @_ @v []
  outruled <- new @_ @v
  write outruled $ RS $ S.singleton 2 --this just causes the outruled list to have one element. Everything should be promoted now...
  scoped @_ @v $ do
    let i = 0
    write v1 ["b"]
    readUpdate outruled (liftIO . putStrLn . (++" outruled in scope0") . show)
    readUpdate v1 (liftIO . putStrLn . (++" in scope0") . show)
    iffb v1 (\c -> c /= ["a"] && c /= []) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    iffb outruled (\(RS s) -> (S.size s == 1) && (not $ S.member i s)) $
      (liftIO . putStrLn  $ "chose "++show i) >> promote v1
    return ()
  scoped @_ @v $ do
    let i = 1
    write v1 ["a"]
    readUpdate outruled (liftIO . putStrLn . (++" outruled in scope1") . show)
    readUpdate v1 (liftIO . putStrLn . (++" in scope1") . show)
    iffb v1 (\c -> c /= ["a"] && c /= []) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    iffb outruled (\(RS s) -> (S.size s == 1) && (not $ S.member i s)) $
      (liftIO . putStrLn  $ "chose "++show i) >> promote v1
    return ()
  --TODO: for some weird reason, the second promote overrides the first one!
  --TODO: outrule value is not propagated up!
  --TODO: outrule value seems to be overridden...
  readUpdate outruled (liftIO . putStrLn . (++" outruled in orig") . show)
  readUpdate v1 (liftIO . putStrLn . (++" in orig") . show)

testRun :: forall v. (v ~ UP) => IO ()
testRun = runPropM @v (testDisj' @_ @(CustPtr (ReaderT (PropState IO v) IO) v))

testPtr :: forall v m a. (StdLat a, MonadProp m v) => m ()
testPtr = new @_ @v @[a] >> return ()
