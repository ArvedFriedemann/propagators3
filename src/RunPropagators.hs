module RunPropagators where

import "this" Propagators
import "this" PropagatorTypes
import "this" CustomVars
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "lattices" Algebra.Lattice
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "monad-var" MonadVar.Instances.IORef
import "base" Data.IORef
import "lens" Control.Lens
import "mtl" Control.Monad.Reader
import "base" Control.Monad.IO.Class
import "base" Control.Concurrent


type PtrCont m a = (a,PCollection m a)

instance (a~b) => HasValue (PtrCont m a) b where
  value = _1

instance (a~b) => HasProps m (PtrCont m a) b where
  props = _2

instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []

newLens' :: forall v a m . (BoundedMeetSemiLattice a, MonadVar m v, PropUtil m, Show a, Eq a) =>
  a -> m (PtrType v (PtrCont m a))
newLens' = newLens value

test1 :: forall v. (v ~ UP) => IO ()
test1 = flip runReaderT initPS $ do
  v1 <- newLens' @v ["a"]
  v2 <- newLens' @v []
  merge v1 v2
  iff v2 (\v -> if null v then NoInstance else Instance) (lift . putStrLn . show)
  return ()

test2 :: forall v. (v ~ UP) => IO ()
test2 = flip runReaderT initPS $ do
  v1 <- newLens' @v $ ["a"]
  write v1 ["b"]
  --liftIO $ threadDelay 1000000
  --readRef v1 >>= liftIO . putStrLn . ("contents of v1: "++) . show
  iff v1 (\v -> if length v == 2 then Instance else NoInstance) (lift . putStrLn . show)
  return ()


test3 :: forall v. (v ~ UP) => IO ()
test3 = flip runReaderT initPS $ do
  v1 <- newLens' @v ["a"]
  scoped $ do
    --TODO: does not propagate up the values from the orig, so the "a" is not present yet
    write v1 ["b"]
    iff v1 (\v -> if length v == 1 then Instance else NoInstance) (lift . putStrLn . (++" in scope") . show)
  iff v1 (\v -> if length v == 1 then Instance else NoInstance) (lift . putStrLn . (++" in orig (this should not be printed!)") .show) --this should not print!
  return ()

{-

X,Y variables
X -m> Y ~> Reason for X, Reason for m implies Reason for Y
A

do
iff X ... iff Y ... iff Z ... write K <~ put reasons X Y Z

k :: m [v a]

do
  p <- newLens' @v ...
  ptrs1 <- withScoped p $ \p' -> k p' >>= ifSucceeds then p' eq p else create learned clause
  ptrs2 <- scope 2 $ k p
  sequence_ $ zipWith dirEq ptrs1 ptrs2

scoped m

iff x nil (...) ;
iff x cons $ splitCons (\x xs -> ) ;

f [] = ...
f (x : xs) = ...

     1
    / \
  2   3
/  \
4  5

IORef (IntMap (val, prop))
IORef (IntMap (IORef (val, prop)))

varA
scopeid 1       2     3 4 5
value   (T,p1) (a,p2) T b T
propagators:
  1 -> ...
  2 -> ...
  3 -> ...


a <- m 1
b <- m 2
getting constraint: m (a -> ... -> z -> m k)






-}
