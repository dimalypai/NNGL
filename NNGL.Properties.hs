module NNGL.Properties where

import NNGL.Base
import NNGL.Picture
import NNGL.Primitives
import NNGL.Combinators
import Test.QuickCheck

newtype Dimension = Dimension Int
  deriving Show

instance Arbitrary Dimension where
  arbitrary = elements (map Dimension [0..100])

newtype Pict = Pict Picture
  deriving Show

instance Arbitrary Pict where
  arbitrary =
    do (Dimension r) <- arbitrary :: Gen Dimension
       (Dimension c) <- arbitrary :: Gen Dimension
       -- from Sudoku Arbitrary
       rows <- sequence [ sequence [ cell | j <- [1..c] ] | i <- [1..r] ]
       return $ Pict rows

cell :: Gen Char
cell = elements [head (head blank), head (head point)]

-- Picture
prop_emptyPicture_dim :: Bool
prop_emptyPicture_dim = width emptyPicture == 0 && height emptyPicture == 0

prop_isSubPictureOf_refl :: Pict -> Bool
prop_isSubPictureOf_refl (Pict pict1) = pict1 `isSubPictureOf` pict1

--prop_isSubPictureOf_trans :: Pict -> Pict -> Pict -> Property
--prop_isSubPictureOf_trans (Pict x) (Pict y) (Pict z) = x `isSubPictureOf` y && y `isSubPictureOf` z ==> x `isSubPictureOf` z

prop_isBlankRect_blank :: Bool
prop_isBlankRect_blank = isBlankRect blank (0, 0) 1 1

prop_isBlankRect_point :: Bool
prop_isBlankRect_point = not $ isBlankRect point (0, 0) 1 1

-- primitives
prop_hline :: Dimension -> Bool
prop_hline (Dimension w) = isCorrectPicture line && width line == w && (w == 0 || height line == 1)
  where line = hline w

prop_vline :: Dimension -> Bool
prop_vline (Dimension h) = isCorrectPicture line && height line == h && (h == 0 || width line == 1)
  where line = vline h

-- general property for rectangles
prop_rect_gen :: Int -> Int -> (Int -> Int -> Picture) -> Property
prop_rect_gen w h rectPrim = w > 0 && h > 0 ==> isCorrectPicture r && width r == w && height r == h
  where r = rectPrim w h

prop_rect :: Dimension -> Dimension -> Property
prop_rect (Dimension w) (Dimension h) = prop_rect_gen w h rect

prop_rect_blanks :: Dimension -> Dimension -> Property
prop_rect_blanks (Dimension w) (Dimension h) = w > 2 && h > 2 ==> innerBlanks `isSubPictureOf` rect w h
  where innerBlanks = vreplicate (h - 2) (hblanks (w - 2))

prop_square :: Dimension -> Bool
prop_square (Dimension s) = isCorrectPicture sq && width sq == height sq && width sq == s
  where sq = square s

prop_square_blanks :: Dimension -> Property
prop_square_blanks (Dimension s) = s > 2 ==> innerBlanks `isSubPictureOf` square s
  where innerBlanks = vreplicate (s - 2) (hblanks (s - 2))

prop_filledRect_rect :: Dimension -> Dimension -> Property
prop_filledRect_rect (Dimension w) (Dimension h) = prop_rect_gen w h filledRect

prop_filledRect :: Dimension -> Dimension -> Bool
prop_filledRect (Dimension w) (Dimension h) = isFilledRect (filledRect w h) (0, 0) w h

prop_filledSquare :: Dimension -> Bool
prop_filledSquare sd@(Dimension s) = prop_square sd && isFilledRect (filledSquare s) (0, 0) s s

-- combinators

-- higher order properties
prop_comb :: Pict -> Pict -> PictureCombinator -> Bool
prop_comb (Pict pict1) (Pict pict2) comb = isCorrectPicture newPict &&
                                           pict1 `isSubPictureOf` newPict &&
                                           pict2 `isSubPictureOf` newPict
  where newPict = pict1 `comb` pict2

prop_comb_dim :: Pict -> Pict -> PictureCombinator -> (Picture -> Int) -> (Picture -> Int) -> Bool
prop_comb_dim p1@(Pict pict1) p2@(Pict pict2) comb dimPlus dimMax = prop_comb p1 p2 comb &&
                                                                    dimPlus newPict == dimPlus pict1 + dimPlus pict2 &&
                                                                    dimMax newPict == max (dimMax pict1) (dimMax pict2)
  where newPict = pict1 `comb` pict2

prop_above_below :: Pict -> Pict -> PictureCombinator -> Bool
prop_above_below p1 p2 comb = prop_comb_dim p1 p2 comb height width

prop_edge :: Pict -> Pict -> PictureCombinator -> Int -> Int -> Bool
prop_edge p1@(Pict pict1) p2@(Pict pict2) comb r c = prop_above_below p1 p2 comb &&
                                                     if pict1W /= pict2W then isBlankRect newPict (r, c) w h else True
  where newPict = pict1 `comb` pict2
        w = abs (pict1W - pict2W)
        h = height $ if pict1W < pict2W then pict1 else pict2
        pict1W = width pict1
        pict2W = width pict2

prop_center :: Pict -> Pict -> PictureCombinator -> Int -> Bool
prop_center p1@(Pict pict1) p2@(Pict pict2) comb r = prop_above_below p1 p2 comb &&
                                                     if pict1W /= pict2W then isBlankRect newPict (r, cl) wl h &&
                                                                              isBlankRect newPict (r, cr) wr h
                                                                         else True
  where newPict = pict1 `comb` pict2
        cl = 0
        wl = floor wBlanks
        h = height $ if pict1W < pict2W then pict1 else pict2
        cr = wl + min pict1W pict2W
        wr = ceiling wBlanks
        pict1W = width pict1
        pict2W = width pict2
        wBlanks = realToFrac (abs (pict1W - pict2W)) / 2.0

-- above
prop_aboveEdge :: Pict -> Pict -> PictureCombinator -> Int -> Bool
prop_aboveEdge p1@(Pict pict1) p2@(Pict pict2) comb c = prop_edge p1 p2 comb (if width pict1 < width pict2 then 0 else height pict1) c

prop_aboveLeft :: Pict -> Pict -> Bool
prop_aboveLeft p1@(Pict pict1) p2@(Pict pict2) = prop_aboveEdge p1 p2 aboveLeft (min (width pict1) (width pict2))

prop_aboveCenter :: Pict -> Pict -> Bool
prop_aboveCenter p1@(Pict pict1) p2@(Pict pict2) = prop_center p1 p2 aboveCenter (if width pict1 < width pict2 then 0 else height pict1)

prop_aboveRight :: Pict -> Pict -> Bool
prop_aboveRight p1 p2 = prop_aboveEdge p1 p2 aboveRight 0

-- below
prop_belowEdge :: Pict -> Pict -> PictureCombinator -> Int -> Bool
prop_belowEdge p1@(Pict pict1) p2@(Pict pict2) comb c = prop_edge p1 p2 comb (if width pict1 < width pict2 then height pict2 else 0) c

prop_belowLeft :: Pict -> Pict -> Bool
prop_belowLeft p1@(Pict pict1) p2@(Pict pict2) = prop_belowEdge p1 p2 belowLeft (min (width pict1) (width pict2))

prop_belowCenter :: Pict -> Pict -> Bool
prop_belowCenter p1@(Pict pict1) p2@(Pict pict2) = prop_center p1 p2 belowCenter (if width pict1 < width pict2 then height pict2 else 0)

prop_belowRight :: Pict -> Pict -> Bool
prop_belowRight p1 p2 = prop_belowEdge p1 p2 belowRight 0

-- beside
prop_beside :: Pict -> Pict -> PictureCombinator -> Bool
prop_beside p1 p2 comb = prop_comb_dim p1 p2 comb width height

prop_besideEdge :: Pict -> Pict -> PictureCombinator -> Int -> Bool
prop_besideEdge p1@(Pict pict1) p2@(Pict pict2) comb r = prop_beside p1 p2 comb &&
                                                         if pict1H /= pict2H then isBlankRect newPict (r, c) w h else True
  where newPict = pict1 `comb` pict2
        c = if pict1H < pict2H then 0 else width pict1
        w = width $ if pict1H < pict2H then pict1 else pict2
        h = abs (pict1H - pict2H)
        pict1H = height pict1
        pict2H = height pict2

prop_besideTop :: Pict -> Pict -> Bool
prop_besideTop p1@(Pict pict1) p2@(Pict pict2) = prop_besideEdge p1 p2 besideTop (min (height pict1) (height pict2))

prop_besideMiddle :: Pict -> Pict -> Bool
prop_besideMiddle p1@(Pict pict1) p2@(Pict pict2) = prop_beside p1 p2 besideMiddle &&
                                                    if pict1H /= pict2H then isBlankRect newPict (rt, c) w ht &&
                                                                             isBlankRect newPict (rb, c) w hb
                                                    else True
  where newPict = pict1 `besideMiddle` pict2
        rt = 0
        c = if pict1H < pict2H then 0 else width pict1
        w = width $ if pict1H < pict2H then pict1 else pict2
        ht = floor hBlanks
        rb = ht + min pict1H pict2H
        hb = ceiling hBlanks
        pict1H = height pict1
        pict2H = height pict2
        hBlanks = realToFrac (abs (pict1H - pict2H)) / 2.0

prop_besideBottom :: Pict -> Pict -> Bool
prop_besideBottom p1 p2 = prop_besideEdge p1 p2 besideBottom 0

-- replication
prop_replicate :: Int -> Picture -> (Int -> Picture -> Picture) -> (Picture -> Int) -> (Picture -> Int) -> Bool
prop_replicate n pict repl dim1 dim2 = isCorrectPicture newPict && dim1 newPict == n * dim1 pict && dim2 newPict == dim2 pict
  where newPict = repl n pict

prop_hreplicate :: Dimension -> Pict -> Property
prop_hreplicate (Dimension n) (Pict pict) = n > 0 ==> prop_replicate n pict hreplicate width height

prop_vreplicate :: Dimension -> Pict -> Property
prop_vreplicate (Dimension n) (Pict pict) = n > 0 ==> prop_replicate n pict vreplicate height width

-- blanks
prop_hblanks :: Dimension -> Bool
prop_hblanks (Dimension w) = isCorrectPicture bl && width bl == w
  where bl = hblanks w

prop_vblanks :: Dimension -> Bool
prop_vblanks (Dimension h) = isCorrectPicture bl && height bl == h
  where bl = vblanks h

-- main
runTests :: IO ()
runTests = do
  runTest "emptyPicture_dim" prop_emptyPicture_dim
  runTest "isSubPictureOf_refl" prop_isSubPictureOf_refl
  --runTest "isSubPictureOf_trans" prop_isSubPictureOf_trans
  runTest "isBlankRect_blank" prop_isBlankRect_blank
  runTest "isBlankRect_point" prop_isBlankRect_point
  runTest "hline" prop_hline
  runTest "vline" prop_vline
  runTest "rect" prop_rect
  runTest "rect_blanks" prop_rect_blanks
  runTest "square" prop_square
  runTest "square_blanks" prop_square_blanks
  runTest "filledRect_rect" prop_filledRect_rect
  runTest "filledRect" prop_filledRect
  runTest "filledSquare" prop_filledSquare
  runTest "aboveLeft" prop_aboveLeft
  runTest "aboveCenter" prop_aboveCenter
  runTest "aboveRight" prop_aboveRight
  runTest "belowLeft" prop_belowLeft
  runTest "belowCenter" prop_belowCenter
  runTest "belowRight" prop_belowRight
  runTest "besideTop" prop_besideTop
  runTest "besideMiddle" prop_besideMiddle
  runTest "besideBottom" prop_besideBottom
  runTest "hreplicate" prop_hreplicate
  runTest "vreplicate" prop_vreplicate
  runTest "hblanks" prop_hblanks
  runTest "vblanks" prop_vblanks

runTest :: Testable p => String -> p -> IO ()
runTest name prop = do
  putStr (name ++ " ")
  quickCheck prop

