{-# LANGUAGE NoImplicitPrelude #-}
module Propagators where

import Prelude hiding (read)
import "monad-var" MonadVar.Classes (MonadMutate, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "lattices" Algebra.Lattice
import "containers" Data.Set (Set)
import qualified "containers" Data.Set as S


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

write :: forall m v k a. (MonadMutate m v, HasValue k a, HasProps k m, Eq a, Lattice a) =>
  v k -> a -> m ()
  -- mutate :: v a -> (a -> (a, b)) -> m b
write adr val = MV.mutate adr update >>= notify
  where
    update :: k -> (k,Set (m ()))
    update v = (setVal v mt, if getVal v == mt then S.empty else getProps v)
      where mt = getVal v /\ val



notify :: (MonadException m) => Set (m ())) -> m (Set (m ())))
notify sets = (S.fromList . concat) <$> forM (S.toList sets) (\s -> catch (s >> return []) (const $ return [s]))

--read p (a -> Maybe a)


{-
-- where m' is the outside monad, which has fail, and m lacks fail
monadNewRunner :: MonadNew m, ... => m a -> m' a
monadNewRunner = lift . id

prop = do
  if bad then fail
  -- can't make new vars here
  monadNewRunner $ do
    ... -- hey I can make new vars here

-----------

data Ready = Ready a | Unready

comp = do
  x <- (read a :: Ready a)
  y <- read b
  if ... (break because of a)
  write c (x + y)

when p pred (\v -> )

if b fails
do
  y <- ...

type PropT m a = ExceptT (v b -> (b -> Bool) -> PropT m a) m a

type ContMT m b a = v a -> (a -> Bool) -> m b

data ContRec m v a b = ContRec{
  crptr :: v a,
  crpred :: (a -> Bool),
  crccont :: (a -> m b)
}

iff :: (MonadRead m v, MonadError (ContRec m v a b) m) => v a -> (a -> Bool) -> m b -> m b
iff p pred m = read p >>= \p' -> if pred p' then m else throw (ContRec p pred m)

blah = do
  at_least x prop1 $ do
    at_least y prop2 $ do
      -- do something with them

-}









--
