module Combinators
    ( module Parser
    , many, many1
    , symbol, anySymbol, string, oneOf
    , digit, natural, integer
    , spaces
    , try
    , endBy, endBy1
    , sepBy, sepBy1
    , foldr1P, foldl1P
    , between, brackets, parens, braces, angles
    ) where

import Parser
import Data.Char

-- Поведение всех комбинаторов описано в тестах в Main.hs.

-- (0.5 балла)
symbol :: (Eq lex) => lex -> Parser lex ()
symbol lex = satisfy (\l -> l == lex) *> pure ()

-- (0.5 балла)
anySymbol :: Parser lex lex
anySymbol = satisfy (\ _ -> True)

-- (0.5 балла)
digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

-- (0.5 балла)
string :: (Eq lex) => [lex] -> Parser lex ()
string str = foldl1 (*>) (map symbol str)

-- (0.5 балла)
oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf syms = foldl1 (<|>) (map (\sym -> satisfy (==sym)) syms)

-- (0.5 балла)
many :: Parser lex a -> Parser lex [a]
many p = ((:) <$> p <*> many p) <|> pure []

-- (0.5 балла)
many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> many p 

-- (0.5 балла)
natural :: Parser Char Integer
natural = digitsToNatural <$> many1 (toInteger <$> digit)
    where
        digitsToNatural digs = foldl1 (\a b -> 10 * a + b) digs

-- (0.5 балла)
integer :: Parser Char Integer
integer = (negate <$> (symbol '-' *> natural)) <|> natural

-- (0.5 балла)
spaces :: Parser Char ()
spaces = (many $ symbol ' ') *> pure ()

-- (0.5 балла)
try :: Parser lex a -> Parser lex (Maybe a)
try p = Just <$> p <|> (pure Nothing)

-- (0.5 балла)
endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy p d = many (p <* d)

-- (0.5 балла)
endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 p d = many1 (p <* d)

-- (0.5 балла)
sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy p d = sepBy1 p d <|> pure []

-- (0.5 балла)
sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 p d = (:) <$> p <*> (many (d *> p))

-- (0.1 балла)
between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between a b c = a *> c <* b

-- (0.1 балла)
brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

-- (0.1 балла)
parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

-- (0.1 балла)
braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

-- (0.1 балла)
angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

-- (1 балл)
foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P fun p q = fun <$> p <*> q <*> ((foldr1P fun p q) <|> p)

-- (1 балл)
foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P fun p q = foldl (\acc (x, y) -> fun acc x y) <$> p <*> many ((,) <$> q <*> p)
