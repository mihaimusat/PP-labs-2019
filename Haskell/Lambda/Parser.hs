{-|
 - Paradigme de Programare CB
 - Laborator 7
 -
 - Funcționalitatea de bază a parserului folosit intern. Nu e nevoie să
 - citiți/înțelegeți implementarea.
 -}
module Parser where

import Data.Char
import Control.Applicative


newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

parse :: Parser a -> String -> Maybe a
parse = (fmap fst .) . runParser


instance Functor Parser where
    fmap f (Parser p) = Parser $ \s -> fmap (\(x, y) -> (f x, y)) $ p s


instance Applicative Parser where
    pure = success

    Parser p <*> Parser p' =
        Parser $ \s -> case p s of
            Just (f, s') -> fmap (\(x, y) -> (f x, y)) $ p' s'
            Nothing      -> Nothing


instance Alternative Parser where
    empty = failure

    Parser p <|> Parser p' = Parser $ \s -> maybe (p' s) Just (p s)


failure :: Parser a
failure = Parser $ const Nothing

success :: a -> Parser a
success r = Parser $ \s -> Just (r, s)

spot :: (Char -> Bool) -> Parser Char
spot p = Parser f
  where
    f "" = Nothing
    f (x : xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

token :: Char -> Parser Char
token = spot . (==)
