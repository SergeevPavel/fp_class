module Tree where

import qualified Data.List as L

-- (2 балла)

data Tree a = Node { value :: a, children :: [Tree a] } deriving Eq

-- show и read должны работать как описано в тестах в Main.hs
instance Show a => Show (Tree a) where
    show (Node x chld) = show x ++ childrenStr
        where
            childrenStr = case chld of []    -> ""
                                       chlds -> ":{" ++ (L.intercalate "," (map show chlds))  ++ "}"

instance Read a => Read (Tree a) where
    readsPrec _ str = undefined 

instance Functor Tree where
    fmap fun (Node a chld) = Node (fun a) (map (fmap fun) chld) 
