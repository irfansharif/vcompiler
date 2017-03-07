module Parser where

import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

import Pretty
import Syntax
import Simplify

languageDef =
  emptyDef { Token.commentStart    = ""
           , Token.commentEnd      = ""
           , Token.commentLine     = ""
           , Token.identStart      = letter
           , Token.identLetter     = letter <|> digit <|> char '_'
           , Token.reservedNames   = ["not", "and", "or"]
           , Token.reservedOpNames = ["<="]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p
                                    -- takes care of the parenthesis and
                                    -- uses p to parse what's inside them
semi       = Token.semi       lexer -- parses a semicolon
symbol     = Token.symbol     lexer -- parses a symbol
whiteSpace = Token.whiteSpace lexer -- parses whitespace

parseProgram :: Parser Program
parseProgram = whiteSpace >> sequenceOfFormula

sequenceOfFormula :: Parser Program
sequenceOfFormula =
  do first <- parseFormula
     rest <- many parseFormula
     return (Seq (first:rest))

parseFormula :: Parser Formula
parseFormula =
  do var  <- parseVar
     reservedOp "<="
     expr <- parseExpr
     semi
     return $ Assign var expr

parseExpr :: Parser Expr
parseExpr = do first <- parseTerm
               rest <- many (reserved "or" >> parseTerm)
               return (Disjunction (first:rest))

parseTerm :: Parser Term
parseTerm = do first <- parseFactor
               rest <- many (reserved "and" >> parseFactor)
               return (Conjunction (first:rest))

parseFactor :: Parser Factor
parseFactor =  FNot <$> (reservedOp "not" >> parseFactor)
           <|> FExpr <$> parens parseExpr
           <|> FConst <$> parseConst
           <|> FVar <$> parseVar

parseConst :: Parser Const
parseConst = 
  do symbol "'"
     i <- symbol "0" <|> symbol "1"
     symbol "'"
     if i == "1" then return One else return Zero 

parseVar :: Parser Var
parseVar =
  do var <- identifier
     return $ Var var 

parseString :: String -> Program
parseString str =
  case parse parseProgram "(F)" str of
    Right r -> r
    Left e  -> error $ show e

parseFile :: String -> IO Program
parseFile file =
  do program  <- readFile file
     case parse parseProgram "(F)" program of
       Right r -> return r
       Left e  -> print e >> fail "parse error"

prettyPrintFile :: String -> IO ()
prettyPrintFile file =
  do program  <- readFile file
     case parse parseProgram "(F)" program of
       Right r -> putStrLn (prettyprint r)
       Left e  -> print e >> fail "parse error"
