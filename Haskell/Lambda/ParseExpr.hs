{-|
 - Paradigme de Programare CB
 - Laborator 7
 -
 - Implementarea parserului de expresii folosit intern. Nu e nevoie să
 - citiți/înțelegeți implementarea.
 -}
module ParseExpr where

import Parser
import Expr
import Control.Applicative
import Data.Char


expr :: Parser Expr
expr = var <|> func <|> app

var :: Parser Expr
var = Var <$> name

func :: Parser Expr
func = Abs <$> absHead <*> expr
    where absHead = lambda *> name <* dot

app :: Parser Expr
app = lparen *> app' <* rparen
    where app' = App <$> (expr <* space) <*> expr


lambda :: Parser Char
lambda = token 'λ' <|> token '\\'

dot :: Parser Char
dot = token '.'

lparen :: Parser Char
lparen = token '('

rparen :: Parser Char
rparen = token ')'

space :: Parser Char
space = token ' '

-- make sure it's ASCII to not parse 'λ' as part of the variable name
name :: Parser String
name = some (spot (\x -> isLower x && isAscii x))
