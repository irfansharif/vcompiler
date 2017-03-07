module Main where

import Parser
import Pretty

import Control.Monad.Trans (liftIO)
import System.Environment
import qualified System.Console.Haskeline as Console
import qualified Text.ParserCombinators.Parsec as Parsec

process :: String -> IO ()
process line = do
    let res = Parsec.parse parseProgram "(F)" line
    case res of
      Right val -> putStrLn (prettyprint val)
      Left err  -> putStrLn ("Err: " ++ show err)

main :: IO ()
main = Console.runInputT Console.defaultSettings loop
  where
  loop = do
    inputline <- Console.getInputLine "(F)~ "
    case inputline of
      Just input -> liftIO (process input) >> loop
      Nothing -> Console.outputStrLn "~fin~"
