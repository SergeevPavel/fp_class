import Control.Monad
import Data.IORef

fac :: Integer -> IO Integer
fac x = do
    r <- newIORef 1
    forM_ [1..(x - 1)] (\i -> modifyIORef r (* i))
    readIORef r
