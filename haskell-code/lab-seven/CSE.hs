{-# LANGUAGE NoMonomorphismRestriction #-}

module CSE (Exp(..),
            -- ExpI(..),
            -- N(..), 
            NodeId, DAG(..),
            -- run_expN,
            mul,
           ) where

import Control.Monad.State
import BiMap
import Syntax
import qualified CSE_ExpI as ExpI (Exp(..))

class Expression e where
  factor :: Factor -> e Factor
  const  :: Const -> e Const
  term   :: Term -> e Term
  expr   :: Expr -> e Expr
  var    :: Var -> e Var

class Exp repr where
   constant :: Int -> repr Int
   variable :: String -> repr Int
   add      :: repr Int -> repr Int -> repr Int

mul :: Exp repr => Int -> repr Int -> repr Int
mul 0 _ = constant 0
mul 1 x = x
mul n x | n `mod` 2 == 0 = mul (n `div` 2) (add x x)
mul n x = add x (mul (n-1) x)

exp_mul8 = mul 8 (variable "i1")

-- * Representing a DAG
-- a collection of Nodes identified by NodeIds,

type NodeId = Int

-- data Node = NConst Int
--           | NVar   String
--           | NAdd   NodeId NodeId
--             deriving (Eq, Ord, Show)

data Node = NConst Const
          | NVar   Var
          | NFactor Factor
          -- | NTerm Term
          | NTerm NodeId NodeId
          -- | NExpr Expr
          | NExpr NodeId NodeId
            deriving (Eq, Ord, Show)

newtype DAG = DAG (BiMap Node) deriving Show

-- * Bottom-up DAG construction
-- As we construct a node for a subexpression, we check if the DAG already
-- has the equal node. If so, we return its |NodeId|; otherwise, we add the node
-- to the DAG.
-- The computation is stateful (we are using the State monad)

-- Hash-consing proper: insert Node into the DAG if it isn't
-- there already, and return its hash code.
hashcons :: Node -> State DAG NodeId
hashcons e = do
  DAG m <- get
  case lookupKey e m of
    Just k  -> return k
    Nothing -> let (k,m') = insert e m
               in put (DAG m') >> return k

-- * Interpreting Exp as a Node in the current DAG
newtype N t = N{unN :: State DAG NodeId}

instance Expression N where
  factor x = N(hashcons $ NFactor x)
  const x  = N(hashcons $ NConst x)
  term (Conjunction x)= N(do
                          xh <- unN (factor (x !! 0))
                          xh' <- unN (factor (x !! 1))
                          hashcons $ NTerm xh xh')
  expr (Disjunction x)= N(do
                          xh <- unN (term (x !! 0))
                          xh' <- unN (term (x !! 1))
                          hashcons $ NExpr xh xh')
  var x    = N(hashcons $ NVar x)

-- instance Exp N where
--   constant x = N(hashcons $ NConst x)
--   variable x = N(hashcons $ NVar x)
--   add e1 e2  = N(do
--                  h1 <- unN e1
--                  h2 <- unN e2
--                  hashcons $ NAdd h1 h2)

-- run the DAG-construction interpreter and the node,
-- as a reference within a DAG.
run_expN :: N t -> (NodeId, DAG)
run_expN (N m) = runState m (DAG empty)

-- We re-interpret exp_mul4 differently this time.
-- The DAG-representation makes the sharing patent

-- A DAG is printed as the list of |(NodeId,Node)| associations. The
-- sharing of the left and right summands below is patent

-- test_sm8 = run_expN exp_mul8
-- (3,DAG BiMap[(0,NVar "i1"),(1,NAdd 0 0),(2,NAdd 1 1),(3,NAdd 2 2)])

-- test_n n = run_expN (mul n (variable "i"))
