module Isomorphism where

import Data.List (intersectBy)
import Syntax

class Isomorphic a where
  (===), (/==) :: a -> a -> Bool
  x /== y      = not (x === y)

instance Isomorphic Program where 
  (Seq x) === (Seq y) = (length (intersectBy (===) x y) == length x) && (length x == length y)

instance Eq Program where 
  a == b = a === b

instance Eq Formula where 
  a == b = a === b

instance Eq Expr where 
  a == b = a === b

instance Ord Expr where
  (Disjunction ts) `compare` (Disjunction ts') = ts `compare` ts' 

instance Eq Term where 
  a == b = a === b

instance Ord Term where
 (Conjunction fs) `compare` (Conjunction fs') = fs `compare` fs' 

instance Eq Factor where 
  a == b = a === b

instance Ord Factor where
  (FNot a) `compare` (FNot b) = a `compare` b
  (FExpr a) `compare` (FExpr b) = a `compare` b
  (FConst a) `compare` (FConst b) = a `compare` b
  (FVar a) `compare` (FVar b) = a `compare` b
  (FVar _) `compare` _ = LT
  (FConst _) `compare` (FVar _) = GT
  (FConst _) `compare` _ = LT
  (FExpr _) `compare` (FVar _) = GT 
  (FExpr _) `compare` (FConst _) = GT
  (FExpr _) `compare` _ = LT
  (FNot _) `compare` _ = GT

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
