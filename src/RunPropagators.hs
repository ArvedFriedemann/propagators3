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

type PtrCont m a = (a,PCollection m a)

instance HasValue (PtrCont m a) a where
  value = _1

instance HasProps m (PtrCont m a) a where
  props = _2

instance Lattice [a] where
  (/\) = (++)
  (\/) = undefined

instance BoundedMeetSemiLattice [a] where
  top = []


test :: IO ()
test = do
  (v1 :: PtrType IORef (PtrCont IO [String])) <- newLens (value @_ @[String]) ["a"]
  (v2 :: PtrType IORef (PtrCont IO [String])) <- newLens (value @_ @[String]) []
  merge @_ @[String] v1 v2
  iff v2 (\(v:: [String]) -> if null v then NoInstance else Instance) (putStrLn . show)
  return ()
