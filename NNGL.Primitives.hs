module NNGL.Primitives where

import NNGL.Base
import NNGL.Picture
import NNGL.Combinators

hline :: Int -> Picture
hline w = hreplicate w point

vline :: Int -> Picture
vline h = vreplicate h point

rect :: Int -> Int -> Picture
rect w h | w == 1 = vline h
         | h == 1 = hline w
         | otherwise = vside `beside` (hside `above` innerBlanks `above` hside) `beside` vside
  where vside = vline h
        hside = hline (w - 2)
        innerBlanks = vreplicate (h - 2) (hblanks (w - 2))

square :: Int -> Picture
square s = rect s s

filledRect :: Int -> Int -> Picture
filledRect w h = vreplicate h (hline w)

filledSquare :: Int -> Picture
filledSquare s = filledRect s s

