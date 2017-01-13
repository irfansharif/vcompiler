module Parser where

import Syntax
import Util
import Pretty

import Control.Monad
import System.Environment
import Text.ParserCombinators.Parsec
import Data.String.Utils
import Data.Char (digitToInt)

readProgram input = case parse parseProgram "(W)" (stripW input) of
  Left err -> putStrLn ("Err: " ++ show err)
  Right val -> putStrLn (ppprogram val)

parseProgram :: Parser Program
parseProgram = do firstWaveform <- parseWaveform
                  rest <- many parseWaveform
                  let program = firstWaveform : rest
                  return (Program program)

parseWaveform :: Parser Waveform
parseWaveform = do identifier <- parseID
                   char ':'
                   bits <- parseBits
                   char ';'
                   return (Waveform identifier bits)

parseID :: Parser ID
parseID = do first <- letter
             rest <- many(letter <|> digit <|> char '_')
             let identifier = first : rest
             return (ID identifier)

parseBits :: Parser Bits
parseBits = do firstBit <- char '0' <|> char '1'
               rest <- many (char '0' <|> char '1')
               let bits = map digitToInt (firstBit:rest)
               return (Bits bits)
