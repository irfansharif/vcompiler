module Pretty (
    ppprogram
) where

import Syntax

import Text.PrettyPrint (Doc, (<>), (<+>))
import qualified Text.PrettyPrint as PP

class Pretty p where
  ppr :: p -> Doc

instance Pretty Program where
  ppr (Program [w]) = PP.text (ppwaveform w)
  ppr (Program (w:ws)) = PP.text (ppwaveform w ++ concatMap ppwaveform ws)

instance Pretty Waveform where
  ppr (Waveform id' bits) = PP.text (ppid id' ++ " : " ++ ppbits bits ++ " ;\n")

instance Pretty ID where
  ppr (ID id') = PP.text id'

instance Pretty Bits where
  ppr (Bits bits) = PP.text (concatMap show bits)

ppprogram :: Program -> String
ppprogram = PP.render . ppr

ppwaveform :: Waveform -> String
ppwaveform = PP.render . ppr

ppid :: ID -> String
ppid = PP.render . ppr

ppbits :: Bits -> String
ppbits = PP.render . ppr
