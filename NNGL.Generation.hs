module NNGL.Generation where

import NNGL.Base
import NNGL.Picture

generate :: Picture -> Nonogram
generate pict = Nonogram rows columns pict
  where rows = generateRows pict
        columns = generateColumns pict

generateRows :: Picture -> [[Int]]
generateRows pict = [ map length (blocksInRow pict r) | r <- rowIndices pict ]

generateColumns :: Picture -> [[Int]]
generateColumns pict = [ map length (blocksInColumn pict c) | c <- columnIndices pict ]
