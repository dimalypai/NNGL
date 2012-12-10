module NNGL.Nonogram where

import NNGL.Base
import NNGL.Picture
import Data.List

instance Show Nonogram where
  show nng = showNonogram nng True

showNonogram :: Nonogram -> Bool -> String
showNonogram (Nonogram rows columns pict) showPict = showColumnsHeader maxLenCol ++ showRows
  where maxLenRow = maximum $ map length rows
        maxLenCol = maximum $ map length columns

        showColumnsHeader 0 = ""
        showColumnsHeader n = startBlanks ++ showColumnHeaderRow n ++ "\n" ++ showColumnsHeader (n-1)
          where startBlanks = blanksFor maxLenRow

        showColumnHeaderRow n = concat [ let col = columns !! c
                                         in if length col >= n
                                            then show (col !! (length col - n)) ++ blankS
                                            else blanksFor 1 | c <- columnIndices pict ]

        -- show all rows for picture (each consists of row header and the picture row)
        showRows = concat [ showRowHeader r ++ (if showPict then showPictureRow r else "") ++ "\n" | r <- rowIndices pict ]

        showRowHeader r = startBlanks ++ concatMap (\r -> show r ++ blankS) row
          where row = rows !! r
                startBlanks = blanksFor (maxLenRow - length row)

        showPictureRow r = intersperse blankC (pict !! r)

        -- returns string with appropriate number of blanks for n significant characters
        blanksFor n = replicate (2 * n) blankC

        blankC = ' '
        blankS = blankC:""

