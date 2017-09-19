module Transpile where

import MIPS
import Syntax

flatten (Seq f) = concatMap flattenfm f
flattenfm (Assign v e) = flattene e
flattene (Disjunction ts) = concatMap flattent ts
flattent (Conjunction fs) = concatMap flattenf fs
flattenf (FExpr e) = flattene e
flattenf (FNot f) = flattenf f
flattenf f@(FConst c) = [f]
flattenf f@(FVar v) = [f]

instrs seq = map f (flatten seq) where
             f factor =
              case factor of
                FConst Zero -> LI("a", 0)
                FConst One -> LI("b", 1)
                FVar v -> MOVE("c", (show v))


