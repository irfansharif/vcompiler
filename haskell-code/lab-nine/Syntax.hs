module Syntax where

data Program  = Seq [Formula] deriving Show
data Formula  = Assign Var Expr deriving Show
data Expr     = Disjunction [Term] deriving Show
data Term     = Conjunction [Factor] deriving Show
data Factor   = FNot Factor
              | FExpr Expr
              | FConst Const
              | FVar Var
              deriving Show
data Const    = Zero | One deriving (Show, Ord, Eq)
data Var      = Var String deriving (Ord, Eq)

instance Show Var where
  show (Var v) = v
