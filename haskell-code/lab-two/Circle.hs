{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Diagrams.TwoD.Text
import Graphics.SVGFonts

example :: Diagram B
example = text' 5 "Hello" # fc black
-- example = fromOffsets [unitX, unitY, 2 *^ unit_X, unit_Y, unitX] # centerXY

parseBits :: (Additive v, Num n, R2 v) => [(Int, Int)] -> [v n]
parseBits [] = []
parseBits (x:xs) = parseBit x ++ parseBits xs
  where parseBit (0,0) = [unitX]
        parseBit (0,1) = [unitY, unitX]
        parseBit (1,0) = [unit_Y, unitX]
        parseBit (1,1) = [unitX]

node :: Int -> Diagram B
node n = topLeftText (show n) # fontSizeL 0.2 # fc black <> circle 2

pairs xs = zip xs (tail xs)
-- main = mainWith (vsep 0.5 [(node 1 ||| (fromOffsets (parseBits (pairs [1,0,1,1,0,1])) :: Diagram B)),(fromOffsets (parseBits (pairs [0,0,0,1,0,0])) :: Diagram B)])
-- main = mainWith (node 4)
-- main = mainWith example
main = renderDia SVG (SVGOptions Absolute Nothing) $ circle 0.2

-- text'   t = stroke (textSVG t 1) # fc purple # fillRule EvenOdd
-- text''  t = stroke (textSVG' (TextOpts lin INSIDE_H KERN False 1 1) t) # fillRule EvenOdd
-- text''' t =        (textSVG_ (TextOpts lin INSIDE_H KERN True  1 1) t) # fillRule EvenOdd

text' d s = (strokeP $ textSVG' (TextOpts lin2 INSIDE_H KERN False d d) s) # lw none
