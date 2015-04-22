-- (0.5 балла)
import Counter

-- Эти две функции отличаются от обычных тем, что, вызывая tick, они считают сколько шагов заняло их выполнение.
filter' :: (a -> Bool) -> [a] -> Counter [a]
filter' _    []     = return []
filter' cond (x:xs) = do
    tick
    if cond x then do
        xs' <- filter' cond xs
        return (x:xs')
    else
        filter' cond xs

append :: [a] -> [a] -> Counter [a]
append []     ys = return ys
append (x:xs) ys = do
    tick
    ys' <- append xs ys
    return (x:ys')

-- Реализуйте qsort через filter' и append
qsort :: Ord a => [a] -> Counter [a]
qsort [] = return []
qsort (x:xs) = do
    left  <- filter' (<= x) xs
    right <- filter' (>  x) xs
    left'  <- qsort left
    right' <- qsort right
    temp <- append [x] right'
    append left' temp

-- Первый вызов должен занимать большее количество тиков ~ в 2 раза
main = do
    print $ runCounter $ qsort [1..15]
    print $ runCounter $ qsort [8,4,12,2,6,10,14,1,3,5,7,9,11,13,15]
