{-# LANGUAGE NoMonomorphismRestriction #-}

module CSE where

import Control.Monad.State
import BiMap
import Syntax
import Ord
import Eq

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

-- * Representing a DAG
-- a collection of Nodes identified by NodeIds,

type NodeId = Int

data Node = NConst Const
          | NVar   Var
          | NFactor NodeId
          | NNFactor NodeId
          | NTerm [NodeId]
          | NExpr [NodeId]
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
  term (Conjunction x)= N(mapM (\x' -> unN (factor x')) x >>= \res ->
                          hashcons $ NTerm res)
  expr (Disjunction x)= N(mapM (\x' -> unN (term x')) x >>= \res ->
                          hashcons $ NExpr res)
  factor (FVar v) = N(unN (var v) >>= \vh ->
                      hashcons $ NFactor vh)
  factor (FConst c) = N(unN (CSE.const c) >>= \ch ->
                        hashcons $ NFactor ch)
  factor (FNot f) = N(unN (factor f) >>= \fh ->
                      hashcons $ NNFactor fh)
  factor (FExpr e) = N(unN (expr e) >>= \eh ->
                       hashcons $ NFactor eh)
  var x    = N(hashcons $ NVar x)
  const x  = N(hashcons $ NConst x)

-- run the DAG-construction interpreter and the node,
-- as a reference within a DAG.
run_expN :: N t -> DAG -> (NodeId, DAG)
run_expN (N m) s = runState m s

-- The DAG-representation makes the sharing patent

-- A DAG is printed as the list of |(NodeId,Node)| associations. The
-- sharing of the left and right summands below is patent
