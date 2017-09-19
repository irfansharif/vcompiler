module Pretty (
    prettyprint
) where

import Iso
import Syntax

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

class Pretty p where
  ppr :: p -> Doc

prettyprint :: (Pretty a) => a -> String
prettyprint = PP.render . ppr

instance Pretty Program where
  ppr (Seq [f]) = PP.text (prettyprint f)
  ppr (Seq (f:fs)) = PP.text (prettyprint f ++ "\n" ++ prettyprint (Seq fs))

instance Pretty Formula where
  ppr (Assign v e) = PP.text (prettyprint v ++ " <= " ++ prettyprint e ++ " ; ")

instance Pretty Expr where 
  ppr (Disjunction [t]) = PP.text (prettyprint t)
  ppr (Disjunction (t:ts)) = PP.text (prettyprint t ++ " or " ++ prettyprint (Disjunction ts))

instance Pretty Term where 
  ppr (Conjunction [f]) = PP.text (prettyprint f)
  ppr (Conjunction (f:fs)) = PP.text (prettyprint f ++ " and " ++ prettyprint (Conjunction fs))

instance Pretty Factor where
  ppr (FNot f) = PP.text ("not" ++ " " ++ prettyprint f)
  ppr (FVar v) = PP.text (prettyprint v)
  ppr (FConst c) = PP.text (prettyprint c)
  ppr (FExpr e) = PP.text ("( " ++ prettyprint e ++ " )")

instance Pretty Var where
  ppr (Var var) = PP.text var

instance Pretty Const where
  ppr Zero = PP.text "'0'"
  ppr One = PP.text "'1'"
