module NNGL.Examples where

import NNGL.Base
import NNGL.Picture
import NNGL.Primitives
import NNGL.Combinators
import NNGL.Generation
import NNGL.Nonogram
import Data.List

bigH = vline 5 `beside` hline 2 `beside` vline 5

bigA = rect 4 3 `above` (vline 2 `beside` hblanks 2 `beside` vline 2)

bigS = hline 4 `aboveLeft` point `above` hline 4 `aboveRight` point `above` hline 4

bigK = vline 5 `beside` point `beside` (point `above` blank `above` point) `beside` (point `above` vblanks 3 `above` point)

bigE = vline 5 `beside` (hline 3 `above` blank `above` hline 3 `above` blank `above` hline 3)

bigL = vline 5 `besideBottom` hline 3

haskell :: Picture
haskell = bigH `beside` blank `beside`
          bigA `beside` blank `beside`
          bigS `beside` blank `beside`
          bigK `beside` blank `beside`
          bigE `beside` blank `beside`
          bigL `beside` blank `beside`
          bigL

lambda :: Picture
lambda = lUp `aboveLeft` (point `above` lMid `above` lLow)
  where lUp = ((blank `beside` hline 2 `beside` blank) `above` (vline 2 `beside` blank `beside` (hline 2 `aboveRight` point)) `beside` blank) `aboveRight` hline 2
        lMid = rect 3 2 `above` (square 2 `beside` blank `beside` (hline 2 `aboveRight` point))
        lLow = ((blank `beside` square 2) `aboveLeft` hline 2) `beside` hblanks 3 `beside` ((hline 2 `beside` blank `beside` point) `aboveRight` (square 2 `besideTop` point))
        
pattern :: Picture 
pattern = patternTop `above` patternBottom
  where leafs = (point `beside` vline 2 `beside` vline 3 `beside` vline 4 `beside` square 5) `belowRight` ((point `above` hline 2 `above` hline 3 `above` hline 4 ) `beside` blank)
        patternTop = leafs `beside` (hflip leafs)
        patternBottom = vflip patternTop
