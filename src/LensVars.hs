{-# LANGUAGE NoImplicitPrelude #-}
module LensVars where

import Prelude hiding (read, pred)
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import qualified "lattices" Algebra.Lattice as Lat
import "lens" Control.Lens.Lens
import "lens" Control.Lens.Setter
import "lens" Control.Lens.Getter
import "either" Data.Either.Combinators
import "containers" Data.IntMap.Strict (IntMap, (!), (!?))
import qualified "containers" Data.IntMap.Strict as IntMap
import "mtl" Control.Monad.Reader.Class
import "mtl" Control.Monad.State
import "base" Control.Monad.IO.Class
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique
import "base" Debug.Trace
import "base" Data.List
import "base" Data.Function
import "base" Control.Monad

--
