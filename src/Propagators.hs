{-# LANGUAGE NoImplicitPrelude #-}
module Propagators where

import Prelude hiding (read)
import "monad-var" MonadVar.Classes (MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S
import "mtl" Control.Monad.Except


type MonadVar m v = (MonadMutate m v, MonadWrite m v, MonadRead m v)

class HasValIn p a where
  getValue :: p -> a
  setValue :: p -> a -> p

class (HasValIn p a) => HasValue p a where
  getVal :: p -> a
  getVal = getValue
  setVal :: p -> a -> p
  setVal = setValue

class (HasValIn p (Set (m ()))) => HasProps p m where
  getProps :: p -> Set (m ())
  getProps = getValue
  setProps :: p -> Set (m ()) -> p
  setProps = setValue

--read p >>= m ~> watch p (read p >>= m)
read :: (MonadRead m v, HasValue k a) => v k -> (a -> m b) -> m b
read adr m = {-TODO: register listener-} (getVal <$> (MV.read adr)) >>= m

write :: forall m v k a.
  (MonadMutate m v, HasValue k a, HasProps k m, Eq a, Lattice a, MonadError (m ()) m) =>
  v k -> a -> m ()
-- TODO: notify returns a set of propagators that haven't fired. Use them.
write adr val = MV.mutate adr update >>= notify >>= undefined
  where
    update :: k -> (k,Set (m ()))
    update v = (setVal v mt, if getVal v == mt then S.empty else getProps v)
      where mt = getVal v /\ val

-- TODO: S.fromList requires an Ord constraint.
notify :: (MonadError (m ()) m) => Set (m ()) -> m (Set (m ()))
-- TODO: Currently this relies on a propagator erroring, but we changed it to
-- instead have propagators return continuations.
notify sets = (S.fromList . concat) <$> forM (S.toList sets)
  (\s -> catchError (s >> return []) (const $ return [s]))

type ContMT m v a b = v a -> (a -> Bool) -> m b

data ContRec m v a b = ContRec {
  crptr :: v a,
  crpred :: (a -> Bool),
  crccont :: ContMT m v a b -- NOTE: I think this is correct now. Was (a -> m b)
}

iff :: (MonadRead m v, HasValue a a, MonadError (ContRec m v a b) m) =>
  v a -> (a -> Bool) -> m b -> m b
iff p pred m = read p $ \p' ->
  if pred p' then m else throwError (ContRec p pred (\pt c -> m))

{-
blah = do
  iff x prop1 $ do
    iff y prop2 $ do
      -- do something with them
-}
