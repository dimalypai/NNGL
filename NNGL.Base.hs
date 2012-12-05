module NNGL.Base where

type Picture = [[Char]]

data Nonogram = Nonogram { rows :: [[Int]], columns :: [[Int]], picture :: Picture }

-- two basic building blocks
blankChar = ' '
pointChar = '*'

blank :: Picture
blank = [[blankChar]]

point :: Picture
point = [[pointChar]]

isBlank :: Char -> Bool
isBlank c = c == blankChar

isPoint :: Char -> Bool
isPoint c = c == pointChar

