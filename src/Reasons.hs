module Reasons where

import Class.MonadProp
import Propagators
import PropagatorTypes

getReasons :: (MonadProp m v, AllReasons xs v) => v a -> m (HList xs)
getReasons = undefined

{-
Note: For now, we just merge all the reasons. Using the threshold function could just reduce the reasons. It is combinatorically hard to express this in an exact way, but a possible better algorithm would be to gradually remove single reasons (so one reason removed at a time) until the threshold function stops fireing.

Our algorithm should just get the set of all variables responsible for the conflict, and then just cut it with the variables present in the goal.
-}

{-}
backtrack :: (MonadProp m v, AllReasons xs v) =>
-- var -> conflict detection -> m (list of reasons)
  v a -> (a -> Instantiated b) -> m (HList xs)
backtrack v _ = getReasons v >>= \ rsn -> do
  concat <$> (sequence $ backtrack <$> (nub $ concat rsn))
-}
