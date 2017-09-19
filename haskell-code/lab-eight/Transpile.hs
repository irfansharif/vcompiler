module Transpile where

import Iso
import Syntax
import Pretty
import Language.Java.Parser
import Language.Java.Lexer
import Language.Java.Pretty
import Language.Java.Syntax
import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

method (Syntax.Assign v e) = do
  let params = map (\var -> FormalParam [Final] (PrimType BooleanT) False (VarId (Ident (prettyprint var)))) (flattene e)
  let traversal = head (preordere e)
  MemberDecl (MethodDecl [Public,Static] [] (Just (PrimType BooleanT)) (Ident (prettyprint v)) params [] Nothing
    (MethodBody (Just 
       (Block [ BlockStmt (Return (Just traversal)) ])
     )
   ))

overall s@(Seq formulas) = do
  CompilationUnit Nothing 
    [
      ImportDecl False (Name [Ident "java",Ident "util"]) True,
      ImportDecl False (Name [Ident "ece351",Ident "w",Ident "ast"]) True,
      ImportDecl False (Name[Ident "ece351",Ident "w",Ident "parboiled"]) True,
      ImportDecl True (Name [Ident "ece351",Ident "util",Ident "Boolean351"]) True,
      ImportDecl False (Name [Ident "ece351",Ident "util",Ident "CommandLine"]) False,
      ImportDecl False (Name [Ident "java",Ident "io",Ident "File"]) False,
      ImportDecl False (Name [Ident "java",Ident "io",Ident "FileWriter"]) False,
      ImportDecl False (Name [Ident "java",Ident "io",Ident "StringWriter"]) False,
      ImportDecl False (Name [Ident "java",Ident "io",Ident "PrintWriter"]) False,
      ImportDecl False (Name [Ident "java",Ident "io",Ident "IOException"]) False,
      ImportDecl False (Name [Ident "ece351",Ident "util",Ident "Debug"]) False
    ] [ClassTypeDecl (ClassDecl [Public,Final] (Ident "Simulator_ex11") [] Nothing [] 
    (ClassBody ([mainb s] ++ (map method formulas)) ))]


mainb (Seq formulas) = do
  let puts = map putstmt formulas
  let loops = map forloop formulas
  MemberDecl (MethodDecl
    [Public,Static] [] 
    Nothing (Ident "main") [FormalParam [Final] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String",[])]))))) False (VarId (Ident "args"))] [] Nothing 
    (MethodBody (Just (Block (puts ++ loops)))))

putstmt f@(Syntax.Assign v e) =
  BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident "output",Ident "put"]) [ Lit (String (prettyprint v)), InstanceCreation [] (TypeDeclSpecifier (ClassType [(Ident "StringBuilder",[])])) [] Nothing])))

forloop f@(Syntax.Assign v e) =
  BlockStmt (BasicFor 
    (Just (ForLocalVars [] (PrimType IntT) [VarDecl (VarId (Ident "time")) (Just (InitExp (Lit (Int 0))))]))
    (Just (BinOp (ExpName (Name [Ident "time"])) LThan (ExpName (Name
      [Ident "timeCount"]))))
    (Just [PostIncrement (ExpName (Name [Ident "time"]))])
    (StmtBlock (loopbody f)))

  -- Block [putstmt, forstmt]


loopbody (Syntax.Assign v e) = do
  let inputs = map (\var -> 
        LocalVars [Final] (PrimType BooleanT) [VarDecl (VarId (Ident ("in_" ++ (prettyprint var)))) 
        (Just (InitExp (MethodInv (MethodCall (Name [Ident "wprogram",Ident "valueAtTime"]) [Lit (String (prettyprint var)),  
                ExpName (Name [Ident "time"])]))))]) (flattene e)
  let flattened_params = map (\var -> ExpName (Name [Ident ("in_" ++ (prettyprint var))]) ) (flattene e)
  let output = LocalVars [Final] (RefType (ClassRefType (ClassType [(Ident "String",[])]))) [VarDecl (VarId (Ident ("out_" ++ (prettyprint v)))) (Just (InitExp (Cond (MethodInv (MethodCall (Name [Ident (prettyprint v)])
                    flattened_params
                  )) (Lit (String "1 ")) (Lit (String "0 ")))))]
  let out_app = BlockStmt (ExpStmt (MethodInv (PrimaryMethodCall
          (MethodInv (MethodCall (Name [Ident "output",Ident "get"]) [Lit (String (prettyprint v))]))
          [] (Ident "append") [ExpName (Name [Ident ("out_" ++ (prettyprint v))])])))

  let block = Block (concat [inputs, [output], [out_app]])
  block

preordere (Disjunction [ts]) = preordert ts
preordere (Disjunction ts) = [MethodInv (MethodCall (Name [Ident "or"]) (concatMap preordert ts))]
preordert (Conjunction [f]) = preorderf f
preordert (Conjunction fs) = [MethodInv (MethodCall (Name [Ident "and"]) (concatMap preorderf fs))]
preorderf (FNot f) = [MethodInv (MethodCall (Name [Ident "not"]) (preorderf f))]
preorderf (FExpr e) = preordere e
preorderf (FVar v) = [ExpName (Name [Ident (prettyprint v)])]
preorderf (FConst c) = if c == Zero then [Lit (Boolean False)] else [Lit (Boolean True)]


flattene (Disjunction ts) = concatMap flattent ts
flattent (Conjunction fs) = concatMap flattenf fs
flattenf (FExpr e) = flattene e
flattenf (FNot f) = flattenf f
flattenf f@(FConst c) = [f]
flattenf f@(FVar v) = [f]
