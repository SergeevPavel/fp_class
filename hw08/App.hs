module App where
-- (2 балла)

import Control.Applicative

data Tree a = Node a [Tree a]

instance Functor Tree where
    fmap f (Node x lst) = Node (f x) (map (fmap f) lst)

instance Applicative Tree where
    pure x = Node x []
    (<*>) (Node f _) tree = f <$> tree
