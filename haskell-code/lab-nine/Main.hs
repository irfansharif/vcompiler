{-# LANGUAGE FlexibleContexts #-}
module Main where

import Parser
import Pretty
import MIPS
import MIPSGenerate
import BiMap
import Control.Monad.Trans (liftIO)
import System.Environment
import qualified Syntax
import qualified System.Console.Haskeline as Console
import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad
import Control.Monad.State

extractast line = do
  let res = Parsec.parse parseProgram "(F)" line
  case res of
    Right val -> val

process :: String -> IO ()
process line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      Right val -> putStrLn (prettyprint val)
      Left err  -> putStrLn ("Err: " ++ show err)

generateMIPS line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      -- Left err -> (Left err)
      -- Right (Syntax.Seq seq) -> Right (snd (mips seq))
      Right (Syntax.Seq seq) -> printInsts (reverse (snd (mips seq)))
      Left err  -> putStrLn ("Err: " ++ show err)

mips seq = foldl (\s f -> 
                  (case f of
                    Syntax.Assign _ ex -> 
                      snd (genMIPS' (MIPSGenerate.expr ex) (DAG empty, [])))) 
                 (DAG empty, []) seq

mips' exp = (genMIPS' (MIPSGenerate.expr exp) (DAG empty, []))
            

main :: IO ()
main = Console.runInputT Console.defaultSettings loop
  where
  loop = do
    inputline <- Console.getInputLine "(F)~ "
    case inputline of
      Just input -> liftIO (process input) >> loop
      Nothing -> Console.outputStrLn "~fin~"
