-- Implicit sharing (common sub-expression elimination) in
-- an embedded DSL for arithmetic expressions
-- The DSL is embedded as a data type

module CSE_ExpI (Exp(..)) where

import BiMap
import Control.Monad.State
import Isomorphism

data Exp
   = Add Exp Exp
   | Variable String
   | Constant Int
   deriving (Eq, Ord, Show)

mul :: Int -> Exp -> Exp
mul 0 _ = Constant 0
mul 1 x = x
mul n x | n `mod` 2 == 0 = mul (n `div` 2) (Add x x)
mul n x = Add x (mul (n-1) x)

-- Examples of duplication
exp_mul4 = mul 4 (Variable "x")
{-
  Add (Add (Variable "x") (Variable "x"))
      (Add (Variable "x") (Variable "x"))
-}

type NodeId = Int
newtype DAG = DAG (BiMap Exp) deriving Show

hashcons :: Exp -> State DAG NodeId
hashcons e = do
  DAG m <- get
  case lookupKey e m of
    Nothing -> let (k,m') = insert e m
               in put (DAG m') >> return k
    Just k  -> return k

-- * Interpreting Exp as a Node in the current DAG
newtype N t = N{unN :: State DAG NodeId}

-- Enter all sub-expressions of an expression into a DAG,
-- removing the duplicates
exp_to_dag :: Exp -> DAG -> (Int, DAG)
exp_to_dag e d@(DAG m) | Just k <- lookupKey e m = (k, d)
exp_to_dag e@(Add e1 e2) d =
    let (_, d1) = exp_to_dag e1 d
        (_, d2) = exp_to_dag e2 d1
        DAG m2  = d2
        (k,m')  = insert e m2
    in (k, DAG m')
-- No sub-expressions
exp_to_dag e d@(DAG m) | Nothing <- lookupKey e m = let (k,m') = insert e m
                                            in (k, DAG m')

-- instance Exp N where
--   constant x = N(hashcons $ NConst x)
--   variable x = N(hashcons $ NVar x)
--   add e1 e2  = N(do
--                  h1 <- unN e1
--                  h2 <- unN e2
--                  hashcons $ NAdd h1 h2)

-- run the DAG-construction interpreter and the node,
-- as a reference within a DAG.

-- test_sm4 = run_expN exp_mul4
-- (2,DAG BiMap[(0,NVar "i1"),(1,NAdd 0 0),(2,NAdd 1 1)])

-- There are only two expressions two compute
dag_4 = exp_to_dag exp_mul4 (DAG empty)
{-
DAG BiMap[(0,Add (Variable "i1") (Variable "i1")),
          (1,Add (Add (Variable "i1") (Variable "i1"))
                 (Add (Variable "i1") (Variable "i1")))]
-}
