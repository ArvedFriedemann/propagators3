module Util where

import "base" Control.Monad

(<<=) :: (Monad m) => m a -> (a -> m b) -> m a
m <<= fm = m >>= \c -> fm c >> return c
