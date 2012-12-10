module NNGL.Picture where

import NNGL.Base
import Data.List

height :: Picture -> Int
height pict | isEmptyPicture pict = 0
            | otherwise           = length pict

width :: Picture -> Int
width pict | isEmptyPicture pict = 0
           | otherwise           = length (head pict)

emptyPicture :: Picture
emptyPicture = [[]]

isEmptyPicture :: Picture -> Bool
isEmptyPicture pict = pict == emptyPicture || null pict

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
isBlankRect pict (r, c) w h = isPropRect pict (r, c) w h isBlank

-- picture -> top left cell -> width -> height
isFilledRect :: Picture -> (Int, Int) -> Int -> Int -> Bool
isFilledRect pict (r, c) w h = isPropRect pict (r, c) w h isPoint

-- picture -> top left cell -> width -> height -> predicate
isPropRect :: Picture -> (Int, Int) -> Int -> Int -> (Char -> Bool) -> Bool
isPropRect pict (r, c) w h prop = and [ prop $ (pict !! i) !! j | i <- [r..r + h - 1], j <- [c..c + w - 1] ]

rowIndices :: Picture -> [Int]
rowIndices pict = [0..height pict-1]

columnIndices :: Picture -> [Int]
columnIndices pict = [0..width pict-1]

pointsInRow :: Picture -> Int -> Int
pointsInRow pict r = length (filter isPoint (pict !! r))

pointsInColumn :: Picture -> Int -> Int
pointsInColumn pict c = length (filter isPoint (transpose pict !! c))

blocksInRow :: Picture -> Int -> [[Char]]
blocksInRow pict r = filter (isPoint . head) (group (pict !! r))

blocksInColumn :: Picture -> Int -> [[Char]]
blocksInColumn pict c = filter (isPoint . head) (group (transpose pict !! c))

showPicture :: Picture -> String
showPicture pict = concatMap (\c -> if c /= '\n' then c:" " else c:"") (unlines pict)

