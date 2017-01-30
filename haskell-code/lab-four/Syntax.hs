module Syntax where

import Data.List (intersectBy)

class Isomorphic a where
  (===), (/==) :: a -> a -> Bool
  x /== y      = not (x === y)

data Program  = Seq [Formula] deriving (Show, Eq)
data Formula  = Assign Var Expr deriving (Show, Eq)
data Expr     = Disjunction [Term] deriving (Show, Eq)
data Term     = Conjunction [Factor] deriving (Show, Eq)
data Factor   = FNot Factor 
              | FExpr Expr
              | FConst Const
              | FVar Var
              deriving (Show, Eq)
data Const    = Zero | One deriving (Show, Eq)
data Var      = Var String deriving (Show, Eq)

instance Isomorphic Program where 
  (Seq x) === (Seq y) = (length (intersectBy (===) x y) == length x) && (length x == length y)

instance Isomorphic Formula where
  (Assign v e) === (Assign v' e') = (v === v') && (e === e')

instance Isomorphic Expr where 
  (Disjunction x) === (Disjunction y) = (length (intersectBy (===) x y) == length x) && (length x == length y)

instance Isomorphic Term where 
  (Conjunction x) === (Conjunction y) = (length (intersectBy (===) x y) == length x) && (length x == length y)

instance Isomorphic Factor where
  (FExpr e)  === (FExpr e')  = e === e'
  (FNot f)   === (FNot f')   = f === f'
  (FVar v)   === (FVar v')   = v === v'
  (FConst c) === (FConst c') = c === c'
  _          === _           = False

instance Isomorphic Const where
  x === y = x == y

instance Isomorphic Var where
  (Var x) === (Var y) = x == y
