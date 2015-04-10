import Data.Char

findDigit :: String -> Maybe Int
findDigit [] = Nothing
findDigit (x:xs) = if isDigit x then
                                    Just $ digitToInt x
                                else
                                    findDigit xs

findDigitAndIndex :: String -> Maybe (Int, Int)
findDigitAndIndex str = go $ zip str [0..]
    where
        go :: [(Char, Int)] -> Maybe (Int, Int)
        go [] = Nothing
        go ((c, i):xs) = if isDigit c then
                                        Just $ (digitToInt c, i)
                                      else
                                        go xs

findDigitAndIndexOrLen :: String -> Either (Int, Int) Int
findDigitAndIndexOrLen str = go $ zip str [0..]
    where
        go :: [(Char, Int)] -> Either (Int, Int) Int
        go [] = Right $ length str
        go ((c, i):xs) = if isDigit c then
                                        Left $ (digitToInt c, i)
                                      else
                                        go xs

data Result = DigitAndIndex Int Int
            | LetterAndIndex Char Int
            | Len Int
            deriving Show

findDigitAndIndexOrLetterAndIndexOrLength :: String -> Result
findDigitAndIndexOrLetterAndIndexOrLength str = go $ zip str [1..]
    where
        go :: [(Char, Int)] -> Result
        go [] = Len $ length str
        go ((c, i):xs) | isDigit c = DigitAndIndex (digitToInt c) i
                       | isLetter c = LetterAndIndex c i
                       | otherwise = go xs

data Nat = Zero | Suc Nat

data Tree a = Leaf a 
            | Branch a (Tree a) (Tree a)

treeHeight :: Tree a -> Int
treeHeight (Leaf x) = 1
treeHeight (Branch x left right) = max (treeHeight left) (treeHeight right) + 1

treeSum :: Num a => Tree a -> a
treeSum (Leaf x) = x
treeSum (Branch x left right) = x + (treeSum left) + (treeSum right)

treeCount :: Tree a -> Int
treeCount (Leaf x) = 1
treeCount (Branch x left right) = 1 + (treeCount left) + (treeCount right)

treeAver :: Tree Float -> Float
treeAver tree = (treeSum tree) / (fromIntegral (treeCount tree))

data Expr = Mul Expr Expr
          | Plus Expr Expr
          | Const Int

eval :: Expr -> Int
eval expr = case expr of
                (Mul l r) -> (eval l) * (eval r)
                (Plus l r) -> (eval l) + (eval r)
                (Const x) -> x
