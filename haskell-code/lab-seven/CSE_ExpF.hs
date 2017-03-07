{-# LANGUAGE NoMonomorphismRestriction #-}

-- * Implicit sharing in the tagless-final style, or:
-- * seemingly pure hash-consing
-- The imperative details of hash-consing are hidden better in a
-- final-tagless style of the DSL embedding, described next.


module CSE_ExpF (Exp(..),
             ExpI(..),
             R(..),
             N(..),NodeId, DAG(..),
             run_expN,
             do_bench,
             mul,
            ) where

import Control.Monad.State
import CSE_BiMap
import qualified CSE_ExpI as ExpI (Exp(..), sklansky)


-- * //
-- * Tagless-final EDSL embedding

-- In the tagless-final approach, embedded DSL
-- expressions are built with `constructor functions' such as |constant|,
-- |variable|, |add| rather than the data constructors |Constant|,
-- |Variable|, |Add|. The constructor functions yield a representation
-- for the DSL expression being built. The representation could be a
-- string (for pretty-printing), an integer (for evaluator), etc. Since
-- the same DSL expression may be concretely represented in several ways,
-- the constructor functions are polymorphic, parameterized by the
-- representation |repr|. In other words, the constructor functions are
-- the members of the type class

class Exp repr where
   constant :: Int -> repr Int
   variable :: String -> repr Int
   add      :: repr Int -> repr Int -> repr Int

-- The data type Exp from ExpI.hs becomes a type class

-- Our expressions here are of only one type, Int. We could have dropped
-- Int and made 'repr' to be a type variable of the kind *.
-- We keep the 'repr Int' notation nevertheless, for consistency
-- with the tagless final paper, and to allow for extensions (e.g.,
-- the addition of booleans).

-- Sample expressions from ExpI.hs now look as follows
exp_a = add (constant 10) (variable "i1")
exp_b = add exp_a (variable "i2")

-- * like those in ExpI.hs modulo the case of the identifiers:
-- * everything is in lower case.

-- The multiplication example:
-- the only two differences is the case of the identifiers
-- and the type signature
mul :: Exp repr => Int -> repr Int -> repr Int
mul 0 _ = constant 0
mul 1 x = x
mul n x | n `mod` 2 == 0 = mul (n `div` 2) (add x x)
mul n x = add x (mul (n-1) x)

exp_mul4 = mul 4 (variable "i1")
exp_mul8 = mul 8 (variable "i1")

-- * Interpreters for our DSL: instances of Exp

-- The first interpreter interprets a tagless-final expression
-- as ExpI.Exp data type, converting the DSL expressions here into
-- the so-called `initial form' of ExpI.hs
-- This is one way to print the tagless-final expressions, since
-- we have derived a Show instance for ExpI.Exp

newtype ExpI t = ExpI (ExpI.Exp)
-- * Question: why do we need the wrapper?

instance Exp ExpI where
    constant = ExpI . ExpI.Constant
    variable = ExpI . ExpI.Variable
    add (ExpI x) (ExpI y) = ExpI (ExpI.Add x y)

test_shb = case exp_b of ExpI e -> e
-- Add (Add (Constant 10) (Variable "i1")) (Variable "i2")

test_sh4 = case exp_mul4 of ExpI e -> e
{-
 Add (Add (Variable "i1") (Variable "i1"))
     (Add (Variable "i1") (Variable "i1"))
-}

-- The conversion to ExpI takes the form of an instance
-- of the class Exp that provides the interpretation for the expression
-- primitives, as the values for the domain ExpI.

-- * Another interpreter: the evaluator
-- That interpretation of tagless-final expressions is not
-- the only one possible. We can write an evaluator, interpreting
-- each expression as an element of the domain R

type REnv = [(String,Int)]
newtype R t = R{unR :: REnv -> t} -- A reader Monad, actually

-- that is, an integer in an environment that gives values for
-- free `variables' that may occur in the expression

instance Exp R where
    constant x = R (\_ -> x)
    variable x = R (\env -> maybe (error $ "no var: " ++ x) id $
                     lookup x env)
    add e1 e2  = R (\env -> unR e1 env + unR e2 env)


test_val4 = unR exp_mul4 [("i1",5)] -- 20
-- * Evaluating sample expressions is good for debugging
-- * Write once (exp_mul4), interpret many times
-- We are using exactly the same exp_mul4 as in
-- test_sh4. We are evaluating it differently. The gist of
-- the final tagless approach is to write an expression once
-- and evaluate it many times.

-- * //
-- ------------------------------------------------------------------------
-- * Detecting implicit sharing (common subexpression elimination)

-- * Goal: detect structurally equal subexpressions and share them,
-- * converting an expression tree into a DAG
-- * Idea: rather than convert an ASTree to ASDag, build the
-- * ASDag to begin with.

-- * Representing a DAG
-- a collection of Nodes identified by NodeIds,

type NodeId = Int

-- We stress: Node is NOT a recursive data structure, so the comparison
-- of Node values takes constant time!
data Node = NConst Int
          | NVar   String
          | NAdd   !NodeId !NodeId
            deriving (Eq,Ord,Show)

-- we could use several bimaps for different operations (for
-- addition, subtraction, etc).

newtype DAG = DAG (BiMap Node) deriving Show

-- * BiMap -- (partial) bijection a <-> Int
-- The mapping between |Node|s and |NodeId|s is realized
-- through a BiMap interface
-- BiMap a establishes a bijection between the values of the type |a|
-- and integers, with the operations to retrieve the value given its key,
-- to find the key for the existing value, and to extend the bijection
-- with a new association.

-- * //
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
  case lookup_key e m of
    Nothing -> let (k,m') = insert e m
               in put (DAG m') >> return k
    Just k  -> return k

-- * Bottom-up DAG construction: tagless-final helps
-- The bottom-up DAG construction maps well to computing a representation
-- for a tagless-final expression, which is also evaluated bottom-up. The
-- DAG construction can therefore be written as a tagless-final
-- interpreter, an instance of the type class Exp. The interpreter maps
-- a tagless-final expression to the concrete representation

-- * Interpreting Exp as a Node in the current DAG
newtype N t = N{unN :: State DAG NodeId}

instance Exp N where
  constant x = N(hashcons $ NConst x)
  variable x = N(hashcons $ NVar x)
  add e1 e2  = N(do
                 h1 <- unN e1
                 h2 <- unN e2
                 hashcons $ NAdd h1 h2)

-- * The state is hidden behind the tagless-final veneer

-- run the DAG-construction interpreter and the node,
-- as a reference within a DAG.
run_expN :: N t -> (NodeId, DAG)
run_expN (N m) = runState m (DAG empty)

-- We re-interpret exp_mul4 differently this time.
-- The DAG-representation makes the sharing patent

-- A DAG is printed as the list of |(NodeId,Node)| associations. The
-- sharing of the left and right summands below is patent

test_sm4 = run_expN exp_mul4
-- (2,DAG BiMap[(0,NVar "i1"),(1,NAdd 0 0),(2,NAdd 1 1)])

test_sm8 = run_expN exp_mul8
-- (3,DAG BiMap[(0,NVar "i1"),(1,NAdd 0 0),(2,NAdd 1 1),(3,NAdd 2 2)])

-- * We have constructed netlists, topologically sorted
-- a netlist is a low-level representation of a circuit listing the gates and
-- their connections, used in circuit manufacturing. Since our BiMap allocated
-- monotonically increasing NodeIds, the resulting netlist comes out
-- topologically sorted. Therefore, we can straightforwardly generate machine
-- code after the standard register allocation.

-- We retain all the information about exp_mul4. In addition, all
-- sharing is fully explicit. As we can see, the evaluation process finds
-- common subexpressions automatically.

-- * //
-- ------------------------------------------------------------------------
-- * Superficially `effectless' common sub-expression elimination

-- sklansky example by Matthew Naylor, with further credit to
-- Chalmers folk, Mary Sheeran and Emil Axelsson.
-- It is said to be similar to scanl1, but contains more parallelism
-- The function sklansky is defined in ExpI.hs

-- * To remind what sklansky produces:
test_sklansky_o n = ExpI.sklansky addition xs
   where
     addition x y = "(" ++ x ++ "+" ++ y ++ ")"
     xs = Prelude.map (("v"++) . show) [1..n]

-- (v1+v2) is used three times
{-
test_sklansky_o 4
["v1","(v1+v2)","((v1+v2)+v3)","((v1+v2)+(v3+v4))"]
-}

-- (v1+v2) is used seven times
{-
test_sklansky_o 8
["v1","(v1+v2)",
 "((v1+v2)+v3)",
 "((v1+v2)+(v3+v4))",
 "(((v1+v2)+(v3+v4))+v5)",
 "(((v1+v2)+(v3+v4))+(v5+v6))",
 "(((v1+v2)+(v3+v4))+((v5+v6)+v7))",
 "(((v1+v2)+(v3+v4))+((v5+v6)+(v7+v8)))"]
-}

-- * sklansky challenge: sharing across expressions, not within an expression

-- * We re-write in the tagless-final style
-- * scratch that: we use sklansky as it was
-- :t ExpI.sklansky
-- ExpI.sklansky :: (a -> a -> a) -> [a] -> [a]
-- Actually, there is no re-write
-- We use Matthew Naylor's code, in pure Haskell, as it was

-- * //
-- We run it differently though
test_sklansky n = runState sk (DAG empty)
  where
  sk = sequence (map unN (ExpI.sklansky add xs))
  xs = map (variable . show) [1..n]

-- Implicit sharing works: hash code 2 (for v1+v2) is used three times

{-
*ExpF> test_sklansky 4
([0,2,4,7],
 DAG BiMap[(0,NVar "1"),(1,NVar "2"),
  (2,NAdd 0 1),(3,NVar "3"),
  (4,NAdd 2 3),(5,NVar "4"),
  (6,NAdd 3 5),(7,NAdd 2 6)])
-}
-- We indeed obtained the set of nodes all pointing within the same DAG
-- The stateful nature was well-hidden till the very end (running)

-- We see the deep sharing: hash code 2, which is v1+v2, is used twice.
-- Then code 7, which is (v1+v2)+(v3+v4), is used 4 times
-- after being computed
{-
*ExpF> test_sklansky 8
([0,2,4,7,9,12,15,19],
 DAG BiMap[(0,NVar "1"),(1,NVar "2"),
 (2,NAdd 0 1),(3,NVar "3"),
 (4,NAdd 2 3),(5,NVar "4"),
 (6,NAdd 3 5),(7,NAdd 2 6),  -- just as it was for test_sklansky 4
 (8,NVar "5"),(9,NAdd 7 8),
 (10,NVar "6"),(11,NAdd 8 10),
 (12,NAdd 7 11),(13,NVar "7"),
 (14,NAdd 11 13),(15,NAdd 7 14),
 (16,NVar "8"),(17,NAdd 13 16),
 (18,NAdd 11 17),(19,NAdd 7 18)])
-}

-- * //
-- ------------------------------------------------------------------------
-- * Performance is still a problem

-- We have demonstrated the sharing detection technique that represents
-- a DSL program as a DAG, eliminating multiple occurrences of common
-- subexpressions. Alas, to find all these common subexpressions we have
-- to examine the entire expression _tree_, which may take long time for large
-- programs (large circuits).
-- See ExpLet.hs for the exponential speed-up.

-- To force the evaluation
do_bench :: (a,DAG) -> Int
do_bench (_,DAG d) = size d

bench_mul n = do_bench $
              run_expN (mul n (variable "i"))


-- It takes effort to find the common subexpressions, just because
-- we have to traverse the whole tree.

bench12 = bench_mul (2^12)
bench13 = bench_mul (2^13)

{-
-- Interpreted code
*ExpF> bench_mul (2^11)
12
(0.06 secs, 4688952 bytes)
*ExpF> bench_mul (2^12)
13
(0.09 secs, 5344760 bytes)
*ExpF> bench_mul (2^13)
14
(0.20 secs, 9791092 bytes)


-- compiled with -O2
Prelude ExpF> bench_mul (2^20)
21
(0.44 secs, 80752592 bytes)
Prelude ExpF> bench_mul (2^21)
22
(0.87 secs, 160434744 bytes)
-}


{-
*ExpF> test_sklansky 128
([0,2,4,7,9,12,15,19,21, ... 560,567,575], ...)
(0.37 secs, 13650184 bytes)
-}

-- Not so bad... The implicit sharing detection is quite fast
bench_skl n = do_bench $ test_sklansky n

{-
-- Interpreted code
*ExpF> bench_skl 128
576
(0.33 secs, 10854396 bytes)
*ExpF> bench_skl 256
1280
(1.79 secs, 38952968 bytes)

-- compiled with -O2
Prelude ExpF> bench_skl 128
576
(0.02 secs, 2163436 bytes)
Prelude ExpF> bench_skl 256
1280
(0.08 secs, 4333380 bytes)
Prelude ExpF> bench_skl 512
2816
(0.37 secs, 13908376 bytes)
-}


main = do
       print test_shb
       print test_sh4

       print test_val4
       print test_sm4
       print test_sm8

       print $ test_sklansky_o 4
       print $ test_sklansky 4
