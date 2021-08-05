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
import "base" Control.Concurrent
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S

testDisj :: forall v. (v ~ UP) => IO ()
testDisj = runPropM @v $ do
  v1 <- new' []
  disjunctForkPromotePred' v1 (\c -> c /= ["a"] && c /= []) [
      traceM "scope0" >> readUpdate v1 (lift . putStrLn . (++" in scope0") . show) >>
        write v1 ["a"] >> traceM "writing in scope0",
      traceM "scope1" >> readUpdate v1 (lift . putStrLn . (++" in scope1") . show) >>
        write v1 ["b"] >> traceM "writing in scope1"
    ]
  readUpdate v1 (lift . putStrLn . (++" in orig") . show)

testDisj' :: forall m v. (MonadIO m, MonadProp m v, PropUtil m) => m ()
testDisj' = do
  v1 <- new' @_ @v []
  getScope >>= \s -> liftIO $ putStrLn $ "currScope: "++show s
  outruled <- new @_ @v
  --write outruled $ RS $ S.singleton 2 --this just causes the outruled list to have one element. Everything should be promoted now...
  scoped @_ @v $ do
    let i = 0
    write v1 ["a"]
    getScope >>= \s -> liftIO $ putStrLn $ "scope1: "++show s
    --write outruled $ RS $ S.singleton 2
    --readUpdate outruled (liftIO . putStrLn . (++" outruled in scope0") . show)
    --readUpdate v1 (liftIO . putStrLn . (++" in scope0") . show)
    iffb v1 (\c -> c /= ["a"] && c /= []) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    --iffb outruled (\(RS s) -> trace ("current outruled : " ++ show s++" matches "++show (S.size s == 1)) (S.size s == 1)) $
      --(liftIO $ threadDelay 1000) >> (liftIO . putStrLn  $ "chose "++show i) >> promote v1
    iff outruled (\(RS s) -> if S.null s then trace "NoInstanceCase" NoInstance else trace "Instance Case" Instance ()) $
      const $ (getScope >>= \s -> liftIO $ putStrLn $ "currScopeExec: "++show s) >> promote v1
      --TODO: scope of execution wrong!
    --promote v1
    --(liftIO $ threadDelay 100)
    --(parScoped @_ @v $ readState outruled) >>= (\v -> write outruled v)
    return ()
  scoped @_ @v $ do
    let i = 1
    write v1 ["b"]
    --readUpdate outruled (liftIO . putStrLn . (++" outruled in scope1") . show)
    --readUpdate v1 (liftIO . putStrLn . (++" in scope1") . show)
    iffb v1 (\c -> c /= ["a"] && c /= []) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    --iffb outruled (\(RS s) -> (S.size s == 1) && (not $ S.member i s)) $
      --(liftIO . putStrLn  $ "chose "++show i) >> promote v1
    return ()
  --TODO: for some weird reason, the second promote overrides the first one!
  --TODO: outrule value is not propagated up!
  --TODO: outrule value seems to be overridden...
  readUpdate outruled (liftIO . putStrLn . (++" outruled in orig") . show)
  readUpdate v1 (liftIO . putStrLn . (++" in orig") . show)
  (liftIO $ threadDelay 2000)
  v1val <- readState v1
  liftIO $ putStrLn $ "delayed v1 in orig: "++show v1val

testRun :: forall v. (v ~ UP) => IO ()
testRun = runPropM @v (testDisj' @_ @(CustPtr (ReaderT (PropState IO v) IO) v))

testPtr :: forall v m a. (StdLat a, MonadProp m v) => m ()
testPtr = new @_ @v @[a] >> return ()
