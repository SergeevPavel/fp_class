-- (0.5 балла)
module Counter
    ( Counter
    , tick
    , runCounter
    ) where

import Control.Applicative
import Control.Monad

-- Монада Counter считает количество тиков, т.е. вызовов функции tick
data Counter a = Counter Int a

-- Возвращает результат вычислений и количество тиков
runCounter :: Counter a -> (a, Int)
runCounter (Counter c x) = (x, c)

instance Functor Counter where
    fmap = liftM
     
instance Applicative Counter where
    pure  = return
    (<*>) = ap

instance Monad Counter where
    return = Counter 0
    (>>=) counter toNext = let (x , c1) = runCounter counter 
                               (x', c2) = runCounter $ toNext x
                           in Counter (c1 + c2) x'

tick :: Counter ()
tick = Counter 1 ()
