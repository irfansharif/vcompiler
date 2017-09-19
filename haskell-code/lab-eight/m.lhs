> import Language.Java.Parser
> import Language.Java.Lexer
> import Language.Java.Pretty
> import Language.Java.Syntax

> m = MemberDecl (MethodDecl [Public,Static] [] 
>  (Just (PrimType BooleanT)) (Ident "x")
>  [
>   FormalParam [Final] (PrimType BooleanT) False (VarId (Ident "a")),
>   FormalParam [Final] (PrimType BooleanT) False (VarId (Ident "b"))
>  ]
>  [] Nothing 
>  (MethodBody
>   (Just 
>    (Block [
>     BlockStmt 
>       (Return 
>         (Just (MethodInv 
>           (MethodCall (Name [Ident "or"]) 
>            [ExpName (Name [Ident "a"]), ExpName (Name [Ident "b"])]
>           ))))
>    ]))
>  ))

public static boolean x(final boolean a, final boolean b) { 
  return or(a, b); 
}


> m' = MemberDecl (MethodDecl
>  [Public,Static] [] 
>  Nothing (Ident "main") [FormalParam [Final] (RefType (ArrayType (RefType (ClassRefType (ClassType [(Ident "String",[])]))))) False (VarId (Ident "args"))] [] Nothing 
>  (MethodBody (Just (Block 
>    [
 
>    BlockStmt (ExpStmt (MethodInv (MethodCall (Name [Ident "output",Ident "put"]) [Lit (String
>            "x"),InstanceCreation [] (TypeDeclSpecifier (ClassType [(Ident
>                  "StringBuilder",[])])) [] Nothing]))),

output.put("x", new StringBuilder());

 
>    BlockStmt (BasicFor 
>      (Just (ForLocalVars [] (PrimType IntT) [VarDecl (VarId (Ident "time")) (Just (InitExp (Lit (Int 0))))]))
>      (Just (BinOp (ExpName (Name [Ident "time"])) LThan (ExpName (Name
>                                                                   [Ident "timeCount"])))) (Just [PostIncrement (ExpName (Name [Ident "time"]))])
>      (StmtBlock (Block [


>        LocalVars [Final] (PrimType BooleanT) 
>        [VarDecl (VarId (Ident "in_a")) (Just (InitExp (MethodInv (MethodCall
>                (Name [Ident "wprogram",Ident "valueAtTime"]) [Lit (String
>                  "a"),ExpName (Name [Ident "time"])]))))],

final boolean in_a = wprogram.valueAtTime("a", time);

>        LocalVars [Final] (PrimType BooleanT) [VarDecl (VarId (Ident "in_b"))
>        (Just (InitExp (MethodInv (MethodCall (Name [Ident "wprogram",Ident
>                                               "valueAtTime"]) [Lit (String "b"),ExpName (Name [Ident
>                                                 "time"])]))))],

final boolean in_b = wprogram.valueAtTime("b", time);

>        LocalVars [Final] (RefType
>          (ClassRefType (ClassType [(Ident "String",[])]))) [VarDecl (VarId (Ident
>              "out_x")) (Just (InitExp (Cond (MethodInv (MethodCall (Name [Ident "x"])
>                      [ExpName (Name [Ident "in_a"]),ExpName (Name [Ident "in_b"])])) (Lit (String "1 ")) (Lit (String "0 ")))))],

final String out_x = x(in_a, in_b) ? "1 " : "0 ";

>        BlockStmt (ExpStmt (MethodInv (PrimaryMethodCall
>              (MethodInv (MethodCall (Name [Ident "output",Ident "get"]) [Lit (String "x")]))
>              [] (Ident "append") [ExpName (Name [Ident "out_x"])])))

output.get("x").append(out_x);

> 
>      ]))
>   )
>   ]))))
