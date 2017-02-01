module Simplify where

import Syntax
import Data.List (any, nub, tails, intersect, (\\))

simplify p = simplify' p (Seq []) -- placeholder
   where simplify' cur last | cur == last = cur
                            | otherwise = let cur' = reduce cur
                                              in simplify' cur' cur

class Reducable a where
  reduce :: a -> a

instance Reducable Program where
  reduce (Seq fs) = Seq (map reduce fs)

instance Reducable Formula where
  reduce (Assign v e) = Assign (reduce v) (reduce e)

instance Reducable Expr where
  reduce (Disjunction ts)
    | Conjunction [FConst Zero] `elem` ts =
      if all (Conjunction[FConst Zero] ==) ts then
        Disjunction [Conjunction [FConst Zero]]
      else
        Disjunction (filter (Conjunction[FConst Zero] /=) ts)
    | Conjunction [FConst One] `elem` ts = Disjunction [Conjunction [FConst One]]
    | repeated ts = Disjunction (nub ts)
    | not (null (ts `intersect` map (\t -> case t of
                                              Conjunction [f] -> Conjunction [FNot f]
                                              _ -> Conjunction []) ts)) = Disjunction [Conjunction [FConst One]]
    | not (null (absorb tcontains ts)) = Disjunction (ts \\ [snd (head (absorb tcontains ts))])
    | not (null (tmerge ts)) = Disjunction (tmerge ts)
    | otherwise = Disjunction (map reduce ts)

instance Reducable Term where
  reduce (Conjunction fs)
    | FConst One `elem` fs =
      if all (FConst One ==) fs then
        Conjunction [FConst One]
      else
        Conjunction (map reduce (filter (FConst One /=) fs))
    | FConst Zero `elem` fs = Conjunction [FConst Zero]
    | repeated fs = Conjunction (map reduce (nub fs))
    | not (null (fs `intersect` map FNot fs)) = Conjunction [FConst Zero]
    | not (null (absorb fcontains fs)) = Conjunction (map reduce (fs \\ [snd (head (absorb fcontains fs))]))
    | not (null (fmerge fs)) = Conjunction (map reduce (fmerge fs))
    | otherwise = Conjunction (map reduce fs)

instance Reducable Factor where
  reduce (FNot (FConst Zero)) = FConst One
  reduce (FNot (FConst One)) = FConst Zero
  reduce (FNot (FNot f)) = f
  reduce (FExpr e) = FExpr (reduce e)
  reduce f = f

instance Reducable Var where
  reduce v = v

instance Reducable Const where
  reduce c = c

tmerge :: [Term] -> [Term]
tmerge ts = nub (concatMap (\(t, t') -> case (t, t') of
    ( _, Conjunction [FExpr (Disjunction [Conjunction _])] )  -> [t, t']
    ( Conjunction [FExpr (Disjunction [Conjunction _])], _ )  -> [t, t']
    ( _, Conjunction [FExpr (Disjunction ts')] )              -> t:ts'
    ( Conjunction [FExpr (Disjunction ts')], _ )              -> t':ts'
    ( _, _ )                                                  -> [t, t']
  ) (allpairs ts))

fmerge :: [Factor] -> [Factor]
fmerge fs = nub (concatMap (\(f, f') -> case (f,f') of
    ( _, FExpr (Disjunction [Conjunction fs']) )              -> f:fs'
    ( FExpr (Disjunction [Conjunction fs']), _ )              -> f':fs'
    ( _, _ ) -> [f,f']
  ) (allpairs fs))

repeated :: (Eq a) => [a] -> Bool
repeated [] = False
repeated (h:t) = h `elem` t || repeated t

tcontains :: Term -> Term -> Bool
tcontains (Conjunction [f]) (Conjunction ts) = any (\t -> case t of
                                                      (FExpr (Disjunction [Conjunction fs])) -> f `elem` fs
                                                      f' -> f == f'
                                                   ) ts
tcontains _ _ = False

allpairs :: (Eq a) => [a] -> [(a, a)]
allpairs xs = [(a,b)| a <- xs, b <- xs, a /= b]
-- allpairs xs = [(a,b)| (a:ys) <- tails xs, b <- ys]

absorb :: (Eq a) => (a -> a -> Bool) -> [a] -> [(a, a)]
absorb contains fs = dropWhile (\(v, e) -> not (contains v e)) (allpairs fs)

fcontains :: Factor -> Factor -> Bool
fcontains v (FExpr (Disjunction fs)) = any (\t -> case t of
                                                    (Conjunction [v']) -> v == v'
                                                    _ -> False
                                                  ) fs
fcontains _ _ = False



-- Seq [Assign (Var "x") (Disjunction [Conjunction [FExpr (Disjunction [Conjunction [FVar (Var "a")],Conjunction[FVar (Var "a"),FVar (Var "b")],Conjunction [FVar (Var "b")]]),FExpr (Disjunction [Conjunction [FVar (Var "b")],Conjunction [FVar (Var "c")],Conjunction [FVar (Var "a")]])],Conjunction [FVar (Var "a"),FVar (Var "a")]])]
-- Conjunction [FExpr (Disjunction [Conjunction [FVar (Var "a")], Conjunction[FVar (Var "a"), FVar (Var "b")],Conjunction [FVar (Var "b")]]),FExpr (Disjunction [Conjunction [FVar (Var "b")],Conjunction [FVar (Var "c")],Conjunction [FVar (Var "a")]])]
