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
  v1 <- new' []
  disjunctForkPromotePred' v1 (\c -> c == ["a"]) [
      write v1 ["a"],
      write v1 ["b"]
    ]
  readUpdate v1 (lift . putStrLn . (++" in scope") . show)
