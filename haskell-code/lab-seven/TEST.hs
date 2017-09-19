{-# LANGUAGE NoMonomorphismRestriction #-}

module TEST where

import BiMap
import CSE

dg = DAG BiMap [ (0,NVar (Var "a")), (1,NFactor 0), (2,NTerm [1]), (3,NVar (Var "b")), (4,NFactor 3), (5,NTerm [4]), (6,NExpr [2,5]) ]
