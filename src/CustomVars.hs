module CustomVars where

import "monad-var" MonadVar.Classes (MonadNew, MonadMutate, MonadMutate_, MonadWrite, MonadRead)
import qualified "monad-var" MonadVar.Classes as MV
import "hashable" Data.Hashable
import "unique" Control.Concurrent.Unique
import "base" Control.Monad.IO.Class
import "base" Data.IORef

newtype UP a = UP (Int, IORef a)

instance Show (UP a) where
  show (UP (u,_)) = show u

instance Eq (UP a) where
  (UP (u1,_)) == (UP (u2,_)) = u1 == u2

instance Ord (UP a) where
  compare (UP (u1,_)) (UP (u2,_)) = compare u1 u2

instance (MonadIO m) => MonadNew m UP where
  new v = liftIO $ do
    u <- newUnique
    ref <- newIORef v
    return $ UP (hash u,ref)

instance (MonadIO m) => MonadRead m UP where
  read (UP (_,r)) = liftIO $ readIORef r

instance (MonadIO m) => MonadWrite m UP where
  write (UP (_,r)) v = liftIO $ atomicWriteIORef r v

instance (MonadIO m) => MonadMutate m UP where
  mutate (UP (_,r)) f = liftIO $ atomicModifyIORef' r f

instance (MonadIO m, MonadMutate m UP) => MonadMutate_ m UP where
  mutate_ p f = MV.mutate p ((,()) . f)





--
