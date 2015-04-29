-- (1.5 балла)
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateIO
    ( StateIO
    , runStateIO, execStateIO, evalStateIO
    ) where

import Control.Monad.State
import Data.IORef

newtype StateIO s a = StateIO { getStateIO :: IORef s -> IO a }

instance Functor (StateIO s) where
    fmap = liftM

instance Applicative (StateIO s) where
    pure  = return
    (<*>) = ap

instance Monad (StateIO s) where
    return x = StateIO $ \_ -> return x
    (>>=) (StateIO st) fun = StateIO $ \ref -> st ref >>= (\x -> let (StateIO fun') = fun x in fun' ref)

instance MonadState s (StateIO s) where
    get = StateIO $ \ref -> readIORef ref
    put x = StateIO $ \ref -> writeIORef ref x

runStateIO :: StateIO s a -> s -> IO (a,s)
runStateIO state x = do
    ref <- newIORef x
    y <- getStateIO state ref
    z <- readIORef ref
    return (y, z)

execStateIO :: StateIO s a -> s -> IO s
execStateIO state x = do
    ref <- newIORef x
    getStateIO state ref
    readIORef ref

evalStateIO :: StateIO s a -> s -> IO a
evalStateIO state x = do
    ref <- newIORef x
    getStateIO state ref
