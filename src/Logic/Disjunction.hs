module Logic.Disjunction where

import "this" Util
import "this" Class.MonadProp
import "this" PropagatorTypes

class Promoter m a where
  promote :: a -> m ()
