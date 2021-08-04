module Main where

import Propagators
import RunPropagators
import RunPropagatorInterface
--import BetterVariables
--import Temp
import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV

main :: IO ()
main = undefined
