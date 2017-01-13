module Util where

import Data.Char (isSpace)

stripW :: String -> String
stripW [] = []
stripW (x:xs) = if isSpace x then stripW xs else x:stripW xs
