module Ord where

import Syntax
import Eq

instance Ord Expr where
  (Disjunction ts) `compare` (Disjunction ts') = ts `compare` ts' 

instance Ord Term where
  (Conjunction fs) `compare` (Conjunction fs') = fs `compare` fs' 

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
