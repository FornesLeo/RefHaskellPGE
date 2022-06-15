module Type where

data Expr = Lit Double
    | Par Expr
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Show, Eq)


eval :: Expr -> Double
eval (Lit x) = x
eval (Par x) = eval x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x / eval y
eval (Pow x y) = eval x ** eval y
