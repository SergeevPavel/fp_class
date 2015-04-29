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

symbol :: (Eq lex) => lex -> Parser lex ()
symbol lex = satisfy (\l -> l == lex) *> pure ()

anySymbol :: Parser lex lex
anySymbol = satisfy (\ _ -> True)

digit :: Parser Char Int
digit = digitToInt <$> satisfy isDigit

string :: (Eq lex) => [lex] -> Parser lex ()
string str = foldl1 (*>) (map symbol str)

oneOf :: (Eq lex) => [lex] -> Parser lex lex
oneOf syms = foldl1 (<|>) (map (\sym -> satisfy (==sym)) syms)

many :: Parser lex a -> Parser lex [a]
many p = ((:) <$> p <*> many p) <|> pure []

many1 :: Parser lex a -> Parser lex [a]
many1 p = (:) <$> p <*> many p 

natural :: Parser Char Integer
natural = digitsToNatural <$> many1 (toInteger <$> digit)
    where
        digitsToNatural digs = foldl1 (\a b -> 10 * a + b) digs

integer :: Parser Char Integer
integer = (negate <$> (symbol '-' *> natural)) <|> natural

spaces :: Parser Char ()
spaces = (many $ symbol ' ') *> pure ()

try :: Parser lex a -> Parser lex (Maybe a)
try p = Just <$> p <|> (pure Nothing)

endBy :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy p d = many (p <* d)

endBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
endBy1 p d = many1 (p <* d)

sepBy :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy p d = sepBy1 p d <|> pure []

sepBy1 :: Parser lex a -> Parser lex b -> Parser lex [a]
sepBy1 p d = (:) <$> p <*> (many (d *> p))

between :: Parser lex a -> Parser lex b -> Parser lex c -> Parser lex c
between a b c = a *> c <* b

brackets :: Parser Char a -> Parser Char a
brackets = between (symbol '[') (symbol ']')

parens :: Parser Char a -> Parser Char a
parens = between (symbol '(') (symbol ')')

braces :: Parser Char a -> Parser Char a
braces = between (symbol '{') (symbol '}')

angles :: Parser Char a -> Parser Char a
angles = between (symbol '<') (symbol '>')

foldr1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldr1P fun p q = fun <$> p <*> q <*> ((foldr1P fun p q) <|> p)

foldl1P :: (a -> b -> a -> a) -> Parser lex a -> Parser lex b -> Parser lex a
foldl1P fun p q = foldl (\acc (x, y) -> fun acc x y) <$> p <*> many ((,) <$> q <*> p)
