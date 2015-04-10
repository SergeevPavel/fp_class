module Parser where

import Control.Applicative


newtype Parser lexeme a = Parser {apply :: [lexeme] -> Either Error (a, [lexeme])}
type Error = String


instance Functor (Parser lexeme) where
    fmap fun p = Parser (\lexs -> (\(x, res) -> (fun x, res)) <$> apply p lexs)


instance Applicative (Parser lexeme) where
    pure x = Parser (\lexs -> Right (x, lexs))
    (<*>) fp p = Parser $ \lexs -> case apply fp lexs of Left err -> Left err
                                                         Right (fun, res) -> (\(x, rres) -> (fun x, rres)) <$> apply p res


--instance Alternative (Parser lexeme) where
--    empty = Parser $ (\lexs -> Left "Empty lexems list")
--    (<|>) 


sepBy :: Alternative f => f a -> f b -> f [a]
sepBy p sep = (:) <$> p <*> ((const id <$> sep <*> sepBy p sep) <|> pure [])
