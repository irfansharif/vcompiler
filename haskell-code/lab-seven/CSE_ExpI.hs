-- Implicit sharing (common sub-expression elimination) in
-- an embedded DSL for arithmetic expressions
-- The DSL is embedded as a data type
--
-- This file is based on the message by Tom Hawkins
-- posted on Haskell-Cafe on Feb 8, 2008
-- http://www.haskell.org/pipermail/haskell-cafe/2008-February/039339.html
-- We fill the missing details so to run the example and exhibit
-- the problem.

module CSE_ExpI (Exp(..), sklansky, bench_mul, bench_skl, main) where

import CSE_BiMap

-- Tom Hawkins:
--   My DSLs invariably define a datatype to capture expressions; something
--   like this:

-- * //
-- * A sub-DSL of arithmetic expressions, embedded as a datatype

data Exp
   = Add Exp Exp			-- one can add Sub, Neg, etc.
   | Variable String
   | Constant Int
   deriving (Eq, Ord, Show)

-- Sample expressions
exp_a = Add (Constant 10) (Variable "i1")
exp_b = Add exp_a (Variable "i2")

-- We can indeed generate assembly or Verilog from Exp after
-- a topological sorting
-- But first we have to detect identical sub-expressions and
-- share then, converting an ASTree to a DAG

-- * Common sub-expressions are abundant and easy to create
-- * Two running examples, absracted from real life

-- * Example 1: multiplication by a known integer constant
-- Our DSL does not have the multiplication
-- operation (many 8-bit CPUs don't have it). We can generate
-- code for constant multiplication by repeated addition.

mul :: Int -> Exp -> Exp
mul 0 _ = Constant 0
mul 1 x = x
mul n x | n `mod` 2 == 0 = mul (n `div` 2) (Add x x)
mul n x = Add x (mul (n-1) x)

-- Examples of duplication
exp_mul4 = mul 4 (Variable "i1")
{-
Add (Add (Variable "i1") (Variable "i1")) (Add (Variable "i1") (Variable "i1"))
-}

exp_mul8 = mul 8 (Variable "i1")
-- Twice as much duplication
{-
Add
 (Add (Add (Variable "i1") (Variable "i1")) (Add (Variable "i1") (Variable "i1")))
 (Add (Add (Variable "i1") (Variable "i1")) (Add (Variable "i1") (Variable "i1")))
-}

-- * Question: why do we see all this duplication?
-- * Wasn't the multiplication algorithm supposed to be optimal?
-- When we apply something like exp_mul8 to an integer 5, do
-- we keep repeating 5+5 four times in a row?

-- * //
-- The other running example:
-- * sklansky by Matthew Naylor, with further credit to
-- * Chalmers folk, Mary Sheeran and Emil Axelsson.
-- * sklansky add [e1,e2, ... en]
-- *   ===> [e1, e1+e2, ... e1+e2+...en]
-- It produces a running sum, with more parallelism and smaller latency.
-- It is somewhat in the spirit of the optimal multiplication mul above
-- The original code, from Matthew Naylor's message:
--  http://www.haskell.org/pipermail/haskell-cafe/2008-February/039671.html


sklansky :: (a -> a -> a) -> [a] -> [a]
sklansky f [] = []
sklansky f [x] = [x]
sklansky f xs = left' ++ [ f (last left') r | r <- right' ]
  where
    (left, right) = splitAt (length xs `div` 2) xs
    left'  = sklansky f left
    right' = sklansky f right


test_sklansky_i n = sklansky Add xs
   where
     xs = Prelude.map (Variable . show) [1..n]

-- * //
-- * test_sklansky_i 4
-- (v1+v2) is repeated three times
{-
  [Variable "1",
   Add (Variable "1") (Variable "2"),
   Add (Add (Variable "1") (Variable "2")) (Variable "3"),
   Add (Add (Variable "1") (Variable "2")) (Add (Variable "3") (Variable "4"))]
-}
-- * Common sub-expressions appear across independent expressions

{-
Tom Hawkins wrote (Haskell-Cafe on Feb 8, 2008)
   The problem comes when I want to generate efficient code from an
   Exp (ie. to C or some other target language).  The method I use
   involves converting the tree of subexpressions into an acyclic graphic
   to eliminate common subexpressions.  The nodes are then topologically
   ordered and assigned an instruction, or statement for each node....

   The process of converting an expression tree to a graph uses either Eq
   or Ord (either derived or a custom instance) to search and build a set
   of unique nodes to be ordered for execution.  In this case "a", then
   "b", then "c".  The problem is expressions often have shared,
   equivalent subnodes, which dramatically grows the size of the tree.

   As these trees grow in size, the equality comparison in graph
   construction quickly becomes the bottleneck for DSL compilation.
   What's worse, the phase transition from tractable to intractable is
   very sharp.  In one of my DSL programs, I made a seemingly small
   change, and compilation time went from milliseconds to
   not-in-a-million-years.
-}
-- Tom Hawkins was about to give up on Haskell
-- His message was entitled: ``I love purity, but it's killing me''

-- *
-- Tom Hawkins's message did not show the algorithm for converting
-- a tree of sub-expressions into a DAG.
-- The message does say that the algorithm uses Eq or Ord instances
-- of Exp
-- Probably Tom Hawkins adds all sub-expressions of an expression
-- into a DAG, ensuring that duplicates are not added

newtype DAG = DAG (BiMap Exp) deriving Show

-- Enter all sub-expressions of an expression into a DAG,
-- removing the duplicates
exp_to_dag :: Exp -> DAG -> DAG
exp_to_dag e d@(DAG m) | Just _ <- lookup_key e m = d
exp_to_dag e@(Add e1 e2) d =
    let d1 = exp_to_dag e1 d
        d2 = exp_to_dag e2 d1
        DAG m2  = d2
        (k,m')  = insert e m2
    in DAG m'
-- No sub-expressions
exp_to_dag e d = d


-- There are only two expressions two compute
dag_4 = exp_to_dag exp_mul4 (DAG empty)
{-
DAG BiMap[(0,Add (Variable "i1") (Variable "i1")),
          (1,Add (Add (Variable "i1") (Variable "i1"))
                 (Add (Variable "i1") (Variable "i1")))]
-}

dag_8 = exp_to_dag exp_mul8 (DAG empty)

bench_mul n =
  case exp_to_dag (mul n (Variable "i")) (DAG empty) of DAG d -> size d

bench20 = bench_mul (2^20)
bench21 = bench_mul (2^21)

-- The size of the DAG increased by 1 node; yet it took twice as
-- much time...
{-
-- Interpreted code
*ExpI> bench20
20
(4.51 secs, 1585728 bytes)
*ExpI> bench21
21
(8.76 secs, 530728 bytes)

-- compiled with -O2
Prelude ExpI> bench_mul (2^20)
20
(0.06 secs, 1053460 bytes)
Prelude ExpI> bench_mul (2^21)
21
(0.11 secs, 1062568 bytes)
Prelude ExpI> bench_mul (2^22)
22
(0.22 secs, 1600568 bytes)
Prelude ExpI> bench_mul (2^23)
23
(0.43 secs, 1068432 bytes)
-}


test_skl_dag = foldl (flip exp_to_dag) (DAG empty) (test_sklansky_i 4)

bench_skl n =
  case foldl (flip exp_to_dag) (DAG empty) (test_sklansky_i n) of
   DAG d -> size d

{-
-- Interpreted code
*ExpI> bench_skl 128
448
(0.32 secs, 1637380 bytes)
*ExpI> bench_skl 256
1024
(1.58 secs, 2234148 bytes)

-- compiled with -O2
Prelude ExpI> bench_skl 128
448
(0.03 secs, 1053144 bytes)
Prelude ExpI> bench_skl 256
1024
(0.12 secs, 2297932 bytes)
Prelude ExpI> bench_skl 512
2304
(0.56 secs, 3419484 bytes)
-}

main = do
       print exp_b
       print exp_mul4
       print exp_mul8
       print $ test_sklansky_i 4
       print dag_8
       print $ bench_skl 128
