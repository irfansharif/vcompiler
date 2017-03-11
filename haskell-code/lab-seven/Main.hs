module Main where

import Parser
import Pretty
import CSE
import BiMap
import Control.Monad.Trans (liftIO)
import System.Environment
import qualified Syntax
import qualified System.Console.Haskeline as Console
import qualified Text.ParserCombinators.Parsec as Parsec
import Control.Monad

process :: String -> IO ()
process line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      Right val -> putStrLn (prettyprint val)
      Left err  -> putStrLn ("Err: " ++ show err)

generateDAG line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      Right (Syntax.Seq seq) -> Right (dag seq)
      Left err -> (Left err)

dag seq = foldl (\dg ex -> 
                  (case ex of
                    Syntax.Assign _ d -> 
                      (snd (run_expN (expr d) dg)))) (DAG empty) seq

main :: IO ()
main = Console.runInputT Console.defaultSettings loop
  where
  loop = do
    inputline <- Console.getInputLine "(F)~ "
    case inputline of
      Just input -> liftIO (process input) >> loop
      Nothing -> Console.outputStrLn "~fin~"
