import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Data.List as L

-- (5 баллов)

-- 1. Определить класс MapLike типов, похожих на Map.
--    В нем должны быть функции empty, lookup, insert, delete, fromList с типами как в Data.Map.
--    Напишите реализацию по умолчанию для fromList.

class MapLike m where
    empty :: m k a
    lookup :: Ord k => k -> m k a -> Maybe a
    insert :: Ord k => k -> a -> m k a -> m k a
    delete :: Ord k => k -> m k a -> m k a
    fromList :: Ord k =>  [(k, a)] -> m k a
    fromList = foldl (\acc (key, val) -> insert key val acc) empty


-- 2. Определить instance MapLike для Data.Map, ListMap и ArrMap
--    Можно использовать любые стандартные функции.

newtype ListMap k v = ListMap [(k,v)]

newtype ArrMap k v = ArrMap (k -> Maybe v)

instance MapLike M.Map where
    empty = M.empty
    lookup = M.lookup
    insert = M.insert
    delete = M.delete
    fromList = M.fromList

instance MapLike ListMap where
    empty = ListMap []
    lookup key (ListMap lst)  = L.lookup key lst
    insert key val lm = case lookup key lm of Nothing -> append key val lm
                                              Just _  -> append key val (delete key lm)
        where
            append :: k -> v -> ListMap k v -> ListMap k v
            append key val (ListMap lst) = ListMap ((key, val):lst)
    delete key (ListMap lst) = ListMap (L.deleteBy (\(k1, _) (k2, _) -> k1 == k2) (key, undefined) lst)
    fromList = ListMap

instance MapLike ArrMap where
    empty = ArrMap (\ _ -> Nothing)
    lookup key (ArrMap map) = map key
    insert key val (ArrMap map) = ArrMap (\nk -> if nk == key then Just val else map nk)
    delete key (ArrMap map) = ArrMap (\nk -> if nk == key then Nothing else map nk)

-- 3. Написать instace Functor для ListMap k и ArrMap k.

instance Functor (ListMap k) where
    fmap f (ListMap lst) = ListMap (map (\(k, v) -> (k, f v)) lst)

instance Functor (ArrMap k) where
    fmap f (ArrMap arrmap) = ArrMap (\key -> fmap f (arrmap key))
