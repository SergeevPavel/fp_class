-- Список экспорта менять нельзя!
module Parser
    ( Parser
    , pure, (<$>), (<$), (<*>), (<*), (*>)
    , empty, (<|>)
    , satisfy, eof
    , evalParser
    , parserTestOK
    , parserTestFail
    ) where

import Control.Applicative
import Test.HUnit
import Data.Foldable(toList)

type Error = Either String -- Можно заменить на Maybe, если есть желание.
newtype Parser lex a = Parser { runParser :: [lex] -> Error (a, [lex]) }

-- (0.5 балла)
evalParser :: Parser lex a -> [lex] -> Error a
evalParser p lexs = fst <$> runParser p lexs 

-- (0.5 балла)
satisfy :: (lex -> Bool) -> Parser lex lex
satisfy pred = Parser check
    where
        check [] = Left "empty list"
        check (l:lexs) = if pred l then Right (l, lexs)
                                   else Left "not satisfy"

-- (0.5 балла)  
eof :: Parser lex ()
eof = Parser $ \lexs -> case lexs of [] -> Right ((), [])
                                     _  -> Left "expected eof"

instance Functor (Parser lex) where
    fmap fun p = Parser $ \lexs -> (\(x, rst) -> (fun x, rst)) <$> runParser p lexs

instance Applicative (Parser lex) where
    -- (0.5 балла)
    pure x = Parser $ \lexs -> Right (x, lexs) 
    -- (1.5 балл)
    (<*>) fp p = Parser $ \lexs -> case runParser fp lexs of Left err -> Left err
                                                             Right (fun, rst) -> go fun rst
        where
            go fun rst  = (\(x, rst') -> (fun x, rst')) <$> runParser p rst

instance Alternative (Parser lex) where
    -- (0.5 балла)
    empty = Parser $ \lexs -> Left "empty"
    -- (0.5 балла)
    (<|>) p1 p2 = Parser $ \lexs -> case runParser p1 lexs of Right (x, rst) -> Right (x, rst)
                                                              _              -> runParser p2 lexs

parserTestOK :: (Eq a, Show a, Eq lex, Show lex) => Parser lex a -> [lex] -> (a, [lex]) -> Test
parserTestOK (Parser p) s r = p s ~?= pure r

parserTestFail :: (Eq a, Show a) => Parser lex a -> [lex] -> Test
parserTestFail (Parser p) s = TestCase $ assertBool "Parser should fail" $ null $ toList (p s)
