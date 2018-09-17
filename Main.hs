module Main where

import Parser
import Pretty
import Simplify

import Control.Monad.Trans (liftIO)
import System.Environment
import qualified System.Console.Haskeline as Console
import qualified Text.ParserCombinators.Parsec as Parsec

process :: String -> IO ()
process line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      Left err -> putStrLn ("Err: " ++ show err)
      Right val -> putStrLn (prettyprint (simplify val))

main :: IO ()
main = Console.runInputT Console.defaultSettings loop
  where
  loop = do
    inputline <- Console.getInputLine "(F)~ "
    case inputline of
      Nothing -> Console.outputStrLn "~fin~"
      Just input -> liftIO (process input) >> loop
