module RunPropagatorInterface where

import "this" Class.MonadProp
import "this" Propagators hiding (new, write, scoped, parScoped)
import "this" CustomVars
import "this" PropagatorTypes
import "this" Logic.Disjunction
import "mtl" Control.Monad.Trans
import "mtl" Control.Monad.Reader

testDisj :: forall v. (v ~ UP) => IO ()
testDisj = runPropM @v $ do
  v <- new' ["a"]
  return ()
  {-}
  v1 <- new []
  disjunctFork' v1 (\(c,_) -> c == ["a"]) [
      write v1 ["a"],
      write v1 ["b"]
    ]
  addPropagator v1 ContinuousInstance (lift . putStrLn . (++" in scope") . show)
  -}
