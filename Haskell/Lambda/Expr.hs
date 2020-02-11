{-|
 - Paradigme de Programare CB
 - Laborator 7
 -
 - Definiția tipului de date care modelează expresii lambda și alte functii
 - utile.
 -}
module Expr where

import qualified Data.Set as S


-- Observați cum tipurile de date algebrice din Haskell, permit definiții foarte
-- ușor de urmărit; puteți observa mai jos exact descrierea întâlnită la curs și
-- la laborator:
--  o expresie lambda poate fi:
--      - o variabilă, caracterizată de un nume
--      - o abstracție, caracterizată de numele argumentului și de corp
--      - o aplicație între două expresii
data Expr = Var String | Abs String Expr | App Expr Expr


instance Show Expr where
    show (Var v) = v
    show (Abs x b) = "λ" ++ x ++ "." ++ show b
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"

-- Implementare unei comparări structurale (i.e. care nu ține cont de numele
-- variabilelor).
instance Eq Expr where
    (Var _) == (Var _) = True
    (Abs _ b) == (Abs _ c) = b == c
    (App e1 e2) == (App f1 f2) = e1 == f1 && e2 == f2
    _ == _ = False


-- Un "Set" este o mulțime neordonată care nu permite duplicate. Structura din
-- Haskell are o implementare mai eficientă decât listele pentru operații ca
-- lookup și delete. Lookup se poate face ca pe liste, cu funcția "elem",
-- datorită polimorfismului parametric.
freeVars :: Expr -> S.Set String
freeVars (Var v) = S.singleton v
freeVars (Abs x b) = S.delete x $ freeVars b
freeVars (App e e') = S.union (freeVars e) (freeVars e')

vars :: Expr -> S.Set String
vars (Var v) = S.singleton v
vars (Abs x b) = vars b
vars (App e e') = S.union (vars e) (vars e')
