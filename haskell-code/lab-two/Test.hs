import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Internal

-- spiqe :: Trail R2
-- spiqe = fromOffsets . map r2 $ [(1,3), (1,-3)]

-- burst = mconcat . take 13 . iterate (rotateBy (-1/13)) $ spiqe

-- colors = cycle [aqua, orange, deeppink, blueviolet, crimson, darkgreen]

-- example :: Diagram Diagrams.Backend.SVG.SVG R2
-- example = lw 1.0 . mconcat  . zipWith lc colors . map stroke . explodeTrail origin $ burst
example :: Diagram B
example = circle 1

main = renderDia SVG (SVGOptions "output.file") example
