> module Parser where

Import the necessary libraries:

> import System.IO
> import Control.Monad
> import Text.ParserCombinators.Parsec
> import Text.ParserCombinators.Parsec.Expr
> import Text.ParserCombinators.Parsec.Language
> import qualified Text.ParserCombinators.Parsec.Token as Token

> import Pretty
> import Syntax
> import Simplify

Having all the data structures (check Syntax.hs) we can go on with writing the
code to do actual parsing. First of all we create the language definition using
Haskell's record syntax and the constructor emptyDef (from
Text.ParserCombinators.Parsec.Language):

> languageDef =
>   emptyDef { Token.commentStart    = ""
>            , Token.commentEnd      = ""
>            , Token.commentLine     = ""
>            , Token.identStart      = letter
>            , Token.identLetter     = letter <|> digit <|> char '_'
>            , Token.reservedNames   = ["not", "and", "or"]
>            , Token.reservedOpNames = ["<="]
>            }

This creates a language definition that accepts the no comments, requires
that the identifiers start with a letter, and contain alphanumeric characters
or |'_'|. Moreover there is a number of reserved names, that cannot be used by
the identifiers.

Having the above definition we can create a lexer:

> lexer = Token.makeTokenParser languageDef

lexer contains a number of lexical parsers, that we can use to parse
identifiers, reserved words/operations, etc. Now we can select/extract them in
the following way:

> identifier = Token.identifier lexer -- parses an identifier
> reserved   = Token.reserved   lexer -- parses a reserved name
> reservedOp = Token.reservedOp lexer -- parses an operator
> parens     = Token.parens     lexer -- parses surrounding parenthesis:
>                                     --   parens p
>                                     -- takes care of the parenthesis and
>                                     -- uses p to parse what's inside them
> semi       = Token.semi       lexer -- parses a semicolon
> symbol     = Token.symbol     lexer -- parses a symbol
> whiteSpace = Token.whiteSpace lexer -- parses whitespace

This isn't really necessary, but should make the code much more readable (also
this is the reason why we used the qualified import of
Text.ParserCombinators.Parsec.Token). Now we can use them to parse the source
code at the token level. One of the nice features of these parsers is that they
take care of all whitespace after the tokens.

As already mentioned a program in this language is simply a sequence of
formulae, so the main parser should basically only parse a sequence of
formulae. But remember to take care of initial whitespace - our parsers only
get rid of whitespace after the tokens!

> parseProgram :: Parser Program
> parseProgram = whiteSpace >> sequenceOfFormula

Now because any formula might be actually a sequence of formulae separated
by semicolon, we use |parseFormula :: Parser Formula| first to parse at least
one statement. We then use the |many| combinator to parse zero or more
subsequent formulae.  The concatenated result is a list of formulae.

> sequenceOfFormula :: Parser Program
> sequenceOfFormula =
>   do first <- parseFormula
>      rest <- many parseFormula
>      return (Seq (first:rest))

Now a single formula is quite simple, it's an assignment from an Expr to a Var.
We therefore parse the Var using |parseVar :: Parser Var|, the assignment
|reservedOp "<="| followed by the expression and the concluding semi-colon.

> parseFormula :: Parser Formula
> parseFormula =
>   do var  <- parseVar
>      reservedOp "<="
>      expr <- parseExpr
>      semi
>      return $ Assign var expr

What's left is to parse the expressions. As before we extract out the first
Term using |parseTerm :: Parser Term| followed by zero or more 
|(reserved "or" >>  Term)|s. The resulting concatenation is a disjunction of
clauses in SAT semantics.

> parseExpr :: Parser Expr
> parseExpr = do first <- parseTerm
>                rest <- many (reserved "or" >> parseTerm)
>                return (Disjunction (first:rest))

Similarlly for Terms we have one or more conjunctive factors, we extract the
first factor using |parseFactor :: Parser Factor| followed by zero or more
|(reserved "and" >> Factors)|.  The resulting concatenation is a conjunction of
clauses in SAT semantics.

> parseTerm :: Parser Term
> parseTerm = do first <- parseFactor
>                rest <- many (reserved "and" >> parseFactor)
>                return (Conjunction (first:rest))

We use <|> to express choice. So a <|> b will first try parser a and if it
fails (but without actually consuming any input) then parser b will be used. 
Note: this means that the order is important.

If you have a parser that might fail after consuming some input, and you still
want to try the next parser, you should look into try combinator. For instance
try p <|> q will try parsing with p and if it fails, even after consuming the
input, the q parser will be used as if nothing has been consumed by p.  Now
let's define the parsers for all the possible statements. This is quite
straightforward as we just use the parsers from the lexer and then use all the
necessary information to create appropriate data structures.

> parseFactor :: Parser Factor
> parseFactor =  FNot <$> (reservedOp "not" >> parseFactor)
>            <|> FExpr <$> parens parseExpr
>            <|> FConst <$> parseConst
>            <|> FVar <$> parseVar

> parseConst :: Parser Const
> parseConst = 
>   do symbol "'"
>      i <- symbol "0" <|> symbol "1"
>      symbol "'"
>      if i == "1" then return One else return Zero 

> parseVar :: Parser Var
> parseVar =
>   do var <- identifier
>      return $ Var var 

And that's it. We have a parser for F!

If you want to experiment with the parser inside ghci, these functions might be
handy:

> parseString :: String -> Program
> parseString str =
>   case parse parseProgram "(F)" str of
>     Left e  -> error $ show e
>     Right r -> r

> parseFile :: String -> IO Program
> parseFile file =
>   do program  <- readFile file
>      case parse parseProgram "(F)" program of
>        Left e  -> print e >> fail "parse error"
>        Right r -> return r

> prettyPrintFile :: String -> IO ()
> prettyPrintFile file =
>   do program  <- readFile file
>      case parse parseProgram "(F)" program of
>        Left e  -> print e >> fail "parse error"
>        Right r -> putStrLn (prettyprint r)

Now you can simply load the module in ghci and then do
|ast <- parseFile "<filename>"| to parse a file and get the
result if parsing was successful. If you already have a string with
the program, you can use parseString.
