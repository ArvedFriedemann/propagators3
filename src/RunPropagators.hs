module RunPropagators where

import "this" Propagators
import "this" LensVars
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "lattices" Algebra.Lattice
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "monad-var" MonadVar.Instances.IORef
import "base" Data.IORef
import "lens" Control.Lens
import "transformers" Control.Monad.Trans.Reader
import "transformers" Control.Monad.Trans.Class

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

newLens' :: forall v a m . (BoundedMeetSemiLattice a, MonadVar m v, HasScope m) =>
  a -> m (PtrType v (PtrCont m a))
newLens' = newLens value

test :: forall v. (v ~ IORef) => IO ()
test = flip runReaderT [0] $ do
  v1 <- newLens' @v ["a"]
  v2 <- newLens' @v []
  merge v1 v2
  iff v2 (\v -> if null v then NoInstance else Instance) (lift . putStrLn . show)
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










-}
