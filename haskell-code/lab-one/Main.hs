module Main where

import Parser
import Util
import Pretty

import Control.Monad.Trans (liftIO)
import System.Environment
import qualified System.Console.Haskeline as Console
import qualified Text.ParserCombinators.Parsec as Parsec

process :: String -> IO ()
process line = do
    let res = Parsec.parse parseProgram "(W)" (stripW line)
    case res of
      Left err -> putStrLn ("Err: " ++ show err)
      Right val -> putStrLn (ppprogram val)

main :: IO ()
main = Console.runInputT Console.defaultSettings loop
  where
  loop = do
    inputline <- Console.getInputLine "(W)~ "
    case inputline of
      Nothing -> Console.outputStrLn "~fin~"
      Just input -> liftIO (process input) >> loop
