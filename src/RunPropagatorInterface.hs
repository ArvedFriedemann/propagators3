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
  v1 <- new' $ RS $ S.empty
  disjunctForkPromotePredFin v1 (\c -> c /= (RS $ S.singleton "a") && c /= RS S.empty) [
      (write v1 $ RS $ S.singleton "a"),
      (write v1 $ RS $ S.singleton "b")
    ] (write v1 $ RS $ S.singleton "BOT!")
  readUpdate v1 (lift . putStrLn . (++" in orig") . show)
  (liftIO $ threadDelay 1000)
  v1val <- readState v1
  liftIO $ putStrLn $ "delayed v1 in orig: "++show v1val

testDisj' :: forall m v. (MonadIO m, MonadProp m v, PropUtil m) => m ()
testDisj' = do
  v1 <- new' @_ @v $ RS $ S.empty
  getScope >>= \s -> liftIO $ putStrLn $ "currScope: "++show s
  outruled <- new @_ @v
  --write outruled $ RS $ S.singleton 2 --this just causes the outruled list to have one element. Everything should be promoted now...
  scoped @_ @v $ do
    let i = 0
    write v1 $ RS $ S.singleton "a"
    getScope >>= \s -> liftIO $ putStrLn $ "scope1: "++show s
    --write outruled $ RS $ S.singleton 2
    --readUpdate outruled (liftIO . putStrLn . (++" outruled in scope0") . show)
    --readUpdate v1 (liftIO . putStrLn . (++" in scope0") . show)
    iffb v1 (\c -> c /= (RS $ S.singleton "a") && c /= RS S.empty) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    iffb outruled (\(RS s) -> (S.size s == 1) && (not $ S.member i s)) $
      (liftIO . putStrLn  $ "chose "++show i) >> promote v1
      --TODO: scope of execution wrong!
      --TODO: Found out that when pushing values upwards, the notify is still in the base scope...
    --promote v1
    --(liftIO $ threadDelay 100)
    --(parScoped @_ @v $ readState outruled) >>= (\v -> write outruled v)
    return ()
  scoped @_ @v $ do
    let i = 1
    write v1 $ RS $ S.singleton "b"
    --readUpdate outruled (liftIO . putStrLn . (++" outruled in scope1") . show)
    --readUpdate v1 (liftIO . putStrLn . (++" in scope1") . show)
    iffb v1 (\c -> c /= (RS $ S.singleton "a") && c /= RS S.empty) $ parScoped @_ @v $ write outruled (RS $ S.singleton i)
    iffb outruled (\(RS s) -> (S.size s == 1) && (not $ S.member i s)) $
      (liftIO . putStrLn  $ "chose "++show i) >> promote v1
    return ()
  readUpdate outruled (liftIO . putStrLn . (++" outruled in orig") . show)
  readUpdate v1 (liftIO . putStrLn . (++" in orig") . show)
  (liftIO $ threadDelay 2000)
  v1val <- readState v1
  liftIO $ putStrLn $ "delayed v1 in orig: "++show v1val

testRun :: forall v. (v ~ UP) => IO ()
testRun = runPropM @v (testDisj' @_ @(CustPtr (ReaderT (PropState IO v) IO) v))

--testPtr :: forall v m a. (StdLat a, MonadProp m v) => m ()
--testPtr = new @_ @v @[a] >> return ()
