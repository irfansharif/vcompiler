{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module SVGPrinter (
    svgprint
) where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import Graphics.SVGFonts

import Syntax

class SVGPrint p where
  svgprint :: p -> Diagram SVG

parseBits :: (Additive v, Num n, R2 v) => [(Int, Int)] -> [v n]
parseBits [] = []
parseBits (x:xs) = parseBit x ++ parseBits xs
  where parseBit (0,0)   = [2 *^ unitX]
        parseBit (0,1)   = [2 *^ unitY, 2 *^  unitX]
        parseBit (1,0)   = [2 *^ unit_Y, 2 *^ unitX]
        parseBit (1,1)   = [2 *^ unitX]
        parseBit (-1, 0) = [unit_Y, 2 *^ unitX]
        parseBit (-1, 1) = [unitY, 2 *^ unitX]

pairs :: [Int] -> [(Int, Int)]
pairs xs = (-1 :: Int, head xs) : zip xs (tail xs)

text' s t = text t # fontSize (local s) <> strutY 0

instance SVGPrint Program where
  svgprint (Program [w]) = svgprint w
  svgprint (Program ws) = vcat (map (\w ->  svgprint w # frame 1) ws)

instance SVGPrint Waveform where
  svgprint (Waveform id' bits) = hcat [svgprint id', strutX 1, svgprint bits]

instance SVGPrint ID where
  svgprint (ID id') = text' 1 id' # fc black  <> square 1 # lw 0

instance SVGPrint Bits where
  svgprint (Bits bits) = fromOffsets (parseBits (pairs bits))
