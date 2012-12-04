module NNGL.Picture where

import NNGL.Base

height :: Picture -> Int
height = length

width :: Picture -> Int
width   [] = 0
width pict = length (head pict)

isCorrectPicture :: Picture -> Bool
isCorrectPicture pict = all (\row -> length row == width pict) pict

isSubPictureOf :: Picture -> Picture -> Bool
pict1 `isSubPictureOf` pict2 | pict1W > pict2W || pict1H > pict2H = False
                             | otherwise = or [ isSubPictFrom r c | r <- [0..pict2H - pict1H], c <- [0..pict2W - pict1W] ]
  where pict1W = width pict1
        pict2W = width pict2
        pict1H = height pict1
        pict2H = height pict2
        isSubPictFrom r c = and [ (pict1 !! (i - r)) !! (j - c) == (pict2 !! i) !! j | i <- [r..r + pict1H - 1], j <- [c..c + pict1W - 1] ]

-- picture -> top left cell -> width -> height
isBlankRect :: Picture -> (Int, Int) -> Int -> Int -> Bool
isBlankRect pict (r, c) w h = and [ isBlank $ (pict !! i) !! j | i <- [r..r + h - 1], j <- [c..c + w - 1] ]

showPicture :: Picture -> String
showPicture pict = concatMap (\c -> if c /= '\n' then c:" " else c:"") (unlines pict)

