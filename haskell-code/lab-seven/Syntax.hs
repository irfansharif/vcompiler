module Syntax where

-- data Program  = Seq [Formula] deriving (Show, Ord, Eq)
-- data Formula  = Assign Var Expr deriving (Show, Ord, Eq)
-- data Expr     = Disjunction [Term] 
--               deriving (Show, Ord, Eq)              -- reuse
-- data Term     = Conjunction [Factor] 
--               deriving (Show, Ord, Eq)              -- reuse
-- data Factor   = FNot Factor                    -- reuse
--               | FExpr Expr
--               | FConst Const
--               | FVar Var
--               deriving (Show, Ord, Eq)
-- data Const    = Zero | One deriving (Show, Ord, Eq) -- reuse
-- data Var      = Var String deriving (Show, Ord, Eq) -- reuse

data Program  = Seq [Formula] deriving Show
data Formula  = Assign Var Expr deriving Show
data Expr     = Disjunction [Term] 
                deriving Show              -- reuse
data Term     = Conjunction [Factor] 
                deriving Show              -- reuse
data Factor   = FNot Factor                    -- reuse
              | FExpr Expr
              | FConst Const
              | FVar Var
              deriving Show
data Const    = Zero | One deriving (Show, Ord, Eq) -- reuse
data Var      = Var String deriving (Show, Ord, Eq) -- reuse
