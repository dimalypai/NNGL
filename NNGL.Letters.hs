module NNGL.Letters where

import NNGL.Base
import NNGL.Picture
import NNGL.Primitives
import NNGL.Combinators
import NNGL.Generation

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

