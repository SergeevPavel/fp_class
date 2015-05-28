-- (2 балла)
module Eval
    ( Eval, runEval
    , Error, Store
    , update, getVar
    ) where

import qualified Data.Map as M
import Data.List
import Control.Monad
import Control.Applicative

import Expr

type Error = String
type Store = M.Map String Value

newtype Eval a = Eval { runEval :: Store -> (Maybe a, [Error], Store) }

instance Functor Eval where
    fmap f (Eval m) = Eval $ \store -> let (res, errors, store') = m store
                                       in  (fmap f res, errors, store')

instance Applicative Eval where
    pure x = Eval $ \store -> (Just x, [], store)
    Eval m <*> Eval k = Eval $ \store -> let (f, errors, store') = m store
                                             (res, errors', store'') = k store'
                                         in  (f <*> res, errors ++ errors', store'')


instance Monad Eval where
    return x = Eval $ \store -> (Just x, [], store)
    Eval m >>= k = Eval $ \store -> let (valm, errors, store') = m store
                                    in  case valm of Just val -> let (valm', errors', store'') = (runEval $ k val) store'
                                                                 in  (valm', errors' ++ errors, store'')
                                                     Nothing  -> (Nothing, errors, store')
    fail err = Eval $ \store -> (Nothing, [err], store)

instance Alternative Eval where
    empty = Eval $ \store -> (Nothing, [], store)
    Eval l <|> Eval r = Eval $ \store -> let (valm, errors, store') = l store
                                         in  case valm of Just _  -> (valm, errors, store')
                                                          Nothing -> r store

-- MonadPlus - аналог Alternative для монад
-- mzero - вычисление, которое ничего не делает, сразу завершается неуспехом
-- mplus m1 m2 пытается выполнить m1, если тот завершился неуспехом, выполняет m2
-- Примеры использования этого класса есть в Utils.hs
instance MonadPlus Eval where
    mzero = Eval $ \store -> (Nothing, [], store)
    mplus (Eval l) (Eval r) = Eval $ \store -> let (valm, errors, store') = l store
                                               in  case valm of Just _  -> (valm, errors, store')
                                                                Nothing -> r store

update :: String -> Value -> Eval ()
update k v = Eval $ \store -> (Just (), [], M.insert k v store)

getVar :: String -> Eval Value
getVar k = Eval $ \store -> case M.lookup k store of Just v  -> (Just v, [], store)
                                                     Nothing -> (Nothing, ["no such variable: " ++ k], store)
