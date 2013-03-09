module Calc.Calc
where

type Number = Integer

data Expr
     = Lit Number
     | Add Expr Expr
     | Sub Expr Expr
     | Mul Expr Expr
     deriving Show

instance Num Expr where
         e1 + e2  = Add e1 e2
         e1 - e2  = Sub e1 e2
         e1 * e2  = Mul e1 e2
         abs e    = e
         signum e = e
         fromInteger = Lit
