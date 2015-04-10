import Data.List

numberOfDigits :: Integer -> Integer
numberOfDigits = toInteger . length . show

f a b c = a (c b)

g = (. flip ($)) . (.)

repl :: Int -> a -> [a]
repl = undefined

ttranspose :: [Char] -> [Char]
ttranspose a = unwords $ map unlines (transpose $ map words (lines a))

mapn :: (Int -> a -> b) -> [a] -> [b]
mapn f lst = go 0 lst
    where 
        go _ []     = []
        go n (x:xs) = (f n x) : (go (n + 1) xs)
