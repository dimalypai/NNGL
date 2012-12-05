module NNGL.Combinators where

import NNGL.Base
import NNGL.Picture
import Data.List

type PictureCombinator = Picture -> Picture -> Picture

-- '-combinators work only with right dimensions

-- above
above' :: PictureCombinator
--pict1 `above'` pict2 | width pict1 /= width pict2 = error "above': Error! The widths of pictures are not equal!"
--                     | otherwise = pict1 ++ pict2
above' = (++)

aboveLeft :: PictureCombinator
pict1 `aboveLeft` pict2 = edgeCombine pict1 pict2 above' (\pict -> pict `beside'` vblanks (height pict)) width

aboveCenter :: PictureCombinator
pict1 `aboveCenter` pict2 =
  centerCombine pict1 pict2 above' (\pict -> vblanks (height pict) `beside'` pict `beside'` vblanks (height pict))
                                   (\pict -> pict `beside'` vblanks (height pict))
                                   width height

aboveRight :: PictureCombinator
pict1 `aboveRight` pict2 = edgeCombine pict1 pict2 above' (\pict -> vblanks (height pict) `beside'` pict) width

above = aboveCenter

-- below
below' :: PictureCombinator
pict1 `below'` pict2 | width pict1 /= width pict2 = error "below': Error! The widths of pictures are not equal!"
                     | otherwise = pict2 `above'` pict1

belowLeft :: PictureCombinator
pict1 `belowLeft` pict2 = pict2 `aboveLeft` pict1

belowCenter :: PictureCombinator
pict1 `belowCenter` pict2 = pict2 `aboveCenter` pict1

belowRight :: PictureCombinator
pict1 `belowRight` pict2 = pict2 `aboveRight` pict1

below = belowCenter

-- beside
beside' :: PictureCombinator
pict1 `beside'` pict2 | height pict1 /= height pict2 = error "beside': Error! The heights of pictures are not equal!"
                      | otherwise = zipWith (++) pict1 pict2

besideTop :: PictureCombinator
pict1 `besideTop` pict2 = edgeCombine pict1 pict2 beside' (\pict -> pict `above'` hblanks (width pict)) height

besideMiddle :: PictureCombinator
pict1 `besideMiddle` pict2 =
  centerCombine pict1 pict2 beside' (\pict -> hblanks (width pict) `above'` pict `above'` hblanks (width pict))
                                    (\pict -> pict `above'` hblanks (width pict))
                                    height width

besideBottom :: PictureCombinator
pict1 `besideBottom` pict2 = edgeCombine pict1 pict2 beside' (\pict -> hblanks (width pict) `above'` pict) height

beside = besideMiddle

-- higher order combinators
edgeCombine :: Picture -> Picture -> PictureCombinator -> (Picture -> Picture) -> (Picture -> Int) -> Picture
edgeCombine pict1 pict2 _    _     _   | isEmptyPicture pict1 = pict2
edgeCombine pict1 pict2 _    _     _   | isEmptyPicture pict2 = pict1
edgeCombine pict1 pict2 comb trans dim | pict1D == pict2D = pict1 `comb` pict2
                                       | pict1D >  pict2D = edgeCombine pict1 (trans pict2) comb trans dim
                                       | otherwise        = edgeCombine (trans pict1) pict2 comb trans dim
  where pict1D = dim pict1
        pict2D = dim pict2

centerCombine :: Picture -> Picture -> PictureCombinator -> (Picture -> Picture) -> (Picture -> Picture) -> (Picture -> Int) -> (Picture -> Int) -> Picture
centerCombine pict1 pict2 _    _      _      _    _    | isEmptyPicture pict1 = pict2
centerCombine pict1 pict2 _    _      _      _    _    | isEmptyPicture pict2 = pict1
centerCombine pict1 pict2 comb trans1 trans2 dim1 dim2 | pict1D1 == pict2D1 = pict1 `comb` pict2
                                                       | pictD1Diff > 0 = if even pictD1Diff
                                                                          then centerCombine pict1 (trans1 pict2) comb trans1 trans2 dim1 dim2
                                                                          else centerCombine pict1 (trans2 pict2) comb trans1 trans2 dim1 dim2
                                                       | pictD1Diff < 0 = if even pictD1Diff
                                                                          then centerCombine (trans1 pict1) pict2 comb trans1 trans2 dim1 dim2
                                                                          else centerCombine (trans2 pict1) pict2 comb trans1 trans2 dim1 dim2
  where pict1D1 = dim1 pict1
        pict2D1 = dim1 pict2
        pict1D2 = dim2 pict1
        pict2D2 = dim2 pict2
        pictD1Diff = pict1D1 - pict2D1

-- infinite combinator
vrepeat :: Picture -> Picture
vrepeat pict = pict `above'` vrepeat pict

-- replication combinators
hreplicate :: Int -> Picture -> Picture
hreplicate n pict | n < 1     = emptyPicture
                  | n == 1    = pict
                  | otherwise = pict `beside'` hreplicate (n - 1) pict

vreplicate :: Int -> Picture -> Picture
vreplicate n pict | n < 1     = emptyPicture
                  | otherwise = take (n * height pict) $ vrepeat pict

-- blanks
hblanks :: Int -> Picture
hblanks w = hreplicate w blank

vblanks :: Int -> Picture
vblanks h = vreplicate h blank

