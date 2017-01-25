> module Syntax where

The grammar for F is defined as follows:
-- Program  → Formula (Formula)* $$
-- Formula  → Var ‘<=’ Expr ‘;’
-- Expr     → Term (‘or’ Term)*
-- Term     → Factor (‘and’ Factor)*
-- Factor   → ‘not’ Factor | ‘(’ Expr ‘)’ | Var | Constant
-- Constant → ‘‘0’’ | ‘‘1’’
-- Var      → id
--
We probably want to parse that into some internal representation of the
language (abstract syntax tree). Therefore we need to define the data
structures for the expressions and statements.

> data Program  = Seq [Formula] deriving (Show)
> data Formula  = Assign Var Expr deriving (Show)
> data Expr     = Disjunction [Term] deriving (Show)
> data Term     = Conjunction [Factor] deriving (Show)
> data Factor   = FNot Factor 
>               | FExpr Expr
>               | FConst Const
>               | FVar Var
>               deriving (Show)
> data Const    = Zero | One deriving (Show)
> data Var      = Var String deriving (Show)
> data BBinOp   = And | Or deriving (Show)
