{-# LANGUAGE FlexibleContexts #-}
module MIPSGenerate where

import Control.Monad.State
import MIPS
import Syntax
import BiMap
import Ord
import Eq

class Expression e where
  expr   :: Expr -> e Expr
  term   :: Term -> e Term
  factor :: Factor -> e Factor
  const  :: Const -> e Const
  var    :: Var -> e Var

-- * Representing a DAG
-- a collection of Nodes identified by NodeIds,

type RegID = Int

data Node = NConst Const
          | NVar   Var
          | NFactor RegID
          | NNFactor RegID
          | NTerm [RegID]
          | NExpr [RegID]
            deriving (Eq, Ord, Show)

newtype DAG = DAG (BiMap Node) deriving Show

-- * Bottom-up DAG construction
-- As we construct a node for a subexpression, we check if the DAG already
-- has the equal node. If so, we return its |RegID|; otherwise, we add the node
-- to the DAG.
-- The computation is stateful (we are using the State monad)

-- Hash-consing proper: insert Node into the DAG if it isn't
-- there already, and return its hash code.
hashcons :: Node -> State (DAG, Code) RegID
hashcons e = do
  (DAG m, c) <- get
  case lookupKey e m of
    Just k  -> return k 
    Nothing -> do let (k,m') = insert e m
                  let c' = case e of
                            (NConst Zero) -> LI("$" ++ (show k), 0):c 
                            (NConst One) -> LI("$" ++ (show k), 1):c 
                            (NVar (Var v)) -> LOAD("$" ++ (show k), v, 0):c
                            (NFactor fid) -> MOVE("$" ++ (show k), "$" ++ (show fid)):c
                            (NNFactor fid) -> UOP("$" ++ (show k), Not, "$" ++ (show fid)):c
                            (NTerm [fid]) -> MOVE("$" ++ (show k), "$" ++ (show fid)):c
                            (NTerm (fid:fid':[])) -> OP("$" ++ (show k), "$" ++ show (fid), 
                                                               And, "$" ++ show (fid')):c
                            (NExpr [tid]) -> MOVE("$" ++ (show k), "$" ++ (show tid)):c
                            (NExpr (tid:tid':[])) -> OP("$" ++ (show k), "$" ++ show (tid), 
                                                               Or, "$" ++ show (tid')):c
                  put (DAG m', c') 
                  return k 

-- -- * Interpreting Exp as a Node in the current DAG
newtype N t = N{unN :: State (DAG, Code) RegID}

instance Expression N where
  expr (Disjunction ts)= N(mapM (\t' -> unN (term t')) ts >>= \tids -> hashcons $ NExpr tids)
  term (Conjunction fs)= N(mapM (\f' -> unN (factor f')) fs >>= \fids -> hashcons $ NTerm fids)
  factor (FExpr e) = N(unN (expr e) >>= \eid -> hashcons $ NFactor eid)
  factor (FNot f) = N(unN (factor f) >>= \fid -> hashcons $ NNFactor fid)
  factor (FVar v) = N(unN (MIPSGenerate.var v) >>= \vid -> hashcons $ NFactor vid)
  factor (FConst c) = N(unN (MIPSGenerate.const c) >>= \cid -> hashcons $ NFactor cid)
  var x    = N(hashcons (NVar x))
  const x  = N(hashcons (NConst x))

-- run the DAG-construction interpreter and the node,
-- as a reference within a DAG.
genMIPS :: N t -> (DAG, Code) -> Code
genMIPS (N m) s = snd (snd (runState m s))

genMIPS' :: N t -> (DAG, Code) -> (RegID, (DAG, Code))
genMIPS' (N m) s = (runState m s)

f x = printInsts (reverse (genMIPS x (DAG empty, [])))
f' x = genMIPS x (DAG empty, [])
-- The DAG-representation makes the sharing patent

-- A DAG is printed as the list of |(RegID,Node)| associations. The
-- sharing of the left and right summands below is patent
