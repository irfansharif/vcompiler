module Iso where

import Data.List (intersectBy)
import Syntax

class Isomorphic a where
  (===), (/==) :: a -> a -> Bool
  x /== y      = not (x === y)

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
