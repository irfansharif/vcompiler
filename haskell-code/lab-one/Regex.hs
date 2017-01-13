import System.Environment
import Text.Regex.Posix
import Data.List.Split

regex =  "([A-Za-z]([A-Za-z0-9_])*( )*:( )*(0|1)(0|1| |\n)*( |\n)*;( |\n)*)+"

main :: IO ()
main = do
  args <- getArgs 
  loop (splitOn " " (head args))

loop :: [String] -> IO ()
loop [] = return ()
loop paths = do
  result <- regexmatch (head paths)
  if result then 
    putStr (head paths ++ ": Pass\n") 
  else 
    putStr (head paths ++ ": Fail\n")
  loop (tail paths)

regexmatch filename = do
  contents <- readFile filename
  return ((contents =~ regex :: String) == contents)
  -- return ((contents =~ regex :: String) == (filter (/= ' ') contents))
