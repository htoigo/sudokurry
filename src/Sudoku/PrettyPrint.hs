module Sudoku.PrettyPrint
  ( renderGrid, Encoding(..)
  ) where

import Data.List (intercalate, intersperse)

import Sudoku (gridOrder, gridSize, Grid(..), Matrix(..), Row(..), Digit,
               digits, blank, groupN)

-- A first pass at rendering a grid.  Here we render directly to a string, but
-- later we'll render to a Doc.
--
-- TODO: Import the HughesPJ pretty printer (package 'pretty') for the 'Doc'
-- type.
-- import Text.PrettyPrint (Doc)
--
-- See: Hughes J., "The Design of a Pretty-printing Library",
--      Wadler P., "A prettier printer"


-- Grid = [Row Digit] = [[Digit]]
-- Row Digit = [Digit]

data Encoding = Ascii | Utf8
              deriving (Eq, Show)

renderGrid :: Encoding -> Grid -> String
renderGrid Ascii = renderGrid' topBtmA boxDivA hlineA renderRowA
renderGrid Utf8  = renderGrid' topBtmU boxDivU hlineU renderRowU

-- | renderGrid' tb div hl rrow
--   tb: the topBtm function (inserts the top & bottom of the outside frame)
--   div: the boxDiv string (the horiz divider between bands)
--   hl: the hline string (the horiz line between rows)
--   rrow: the renderRow function
renderGrid' :: ([String] -> [String]) -> String -> String
                 -> (Row Digit -> String) -> Grid -> String
renderGrid' tb div hl rrow =
  concat . tb . intersperse div . map (intercalate hl)
    . groupN gridOrder . map rrow

-- | topBtmA is the ascii version of the topBtm function, which inserts the top
--   and bottom of the outside frame.  Grids rendered in ascii do not have a top
--   and bottom frame.
topBtmA :: [String] -> [String]
topBtmA = id

-- | topBtmU is the unicode version of the topBtm function (inserts the top &
--   bottom of the outside frame).
topBtmU :: [String] -> [String]
topBtmU rs = topFrm : (rs ++ [btmFrm])
  where topFrm = "╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗\n"
  
        btmFrm = "╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝\n"

-- | renderRowA renders the given row, in ascii.
renderRowA :: Row Digit -> String
renderRowA = (++"\n") . concat . insertEvery gridOrder "|" . map renderDigitA

-- | renderRowU renders the given row, in unicode (utf8).
renderRowU :: Row Digit -> String
renderRowU = (++"\n") . concat . intersperseFL "║" . map (intercalate "│")
               . groupN gridOrder . map renderDigitU

-- | instersperseFL x xs is similar to intersperse, but in addition to adding
--   the element x between the elements of xs, it also adds x to the beginning
--   and end of xs.  'FL' stands for 'first' and 'last'.
intersperseFL :: a -> [a] -> [a]
intersperseFL x xs = x : (intersperse x xs) ++ [x]

-- | renderDigitA renders the given digit in ascii.
renderDigitA :: Digit -> String
renderDigitA d
    | blank d         = ". "
    | d `elem` digits = [d, ' ']
    | otherwise       = ""

-- | renderDigitU renders the given digit in unicode.
renderDigitU :: Digit -> String
renderDigitU d
    | blank d         = "   "
    | d `elem` digits = [' ', d, ' ']
    | otherwise       = ""

-- | hlineA renders the dividing line between rows, but grids rendered in ascii
--   do not have this dividing line.
hlineA :: String
hlineA = ""

-- | hlineU renders the dividing line between rows, for unicode-rendered grids.
hlineU :: String
hlineU = "╟───┼───┼───╫───┼───┼───╫───┼───┼───╢\n"

-- | boxDivA renders the horizontal line dividing boxes, or bands, in ascii.
boxDivA :: String
boxDivA = "------+------+------\n"

-- | boxDivU renders the horizontal line dividing boxes, or bands, in unicode.
boxDivU :: String
boxDivU = "╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣\n"


-- | insertEvery n sep, applied to a list xs, inserts the element
--   sep into xs after every n items.
insertEvery :: Int -> a -> [a] -> [a]
insertEvery n sep  = intercalate [sep] . groupN n


-- | renderCMatrix renders a choice matrix--i.e., a matrix of lists of digits.
renderCMatrix :: Matrix [Digit] -> String
renderCMatrix = undefined


{-
Possible grid layouts:

 A1 A2 A3| A4 A5 A6| A7 A8 A9    4 . . |. . . |8 . 5     4 1 7 |3 6 9 |8 2 5 
 B1 B2 B3| B4 B5 B6| B7 B8 B9    . 3 . |. . . |. . .     6 3 2 |1 5 8 |9 4 7
 C1 C2 C3| C4 C5 C6| C7 C8 C9    . . . |7 . . |. . .     9 5 8 |7 2 4 |3 1 6 
---------+---------+---------    ------+------+------    ------+------+------
 D1 D2 D3| D4 D5 D6| D7 D8 D9    . 2 . |. . . |. 6 .     8 2 5 |4 3 7 |1 6 9 
 E1 E2 E3| E4 E5 E6| E7 E8 E9    . . . |. 8 . |4 . .     7 9 1 |5 8 6 |4 3 2 
 F1 F2 F3| F4 F5 F6| F7 F8 F9    . . . |. 1 . |. . .     3 4 6 |9 1 2 |7 5 8 
---------+---------+---------    ------+------+------    ------+------+------
 G1 G2 G3| G4 G5 G6| G7 G8 G9    . . . |6 . 3 |. 7 .     2 8 9 |6 4 3 |5 7 1 
 H1 H2 H3| H4 H5 H6| H7 H8 H9    5 . . |2 . . |. . .     5 7 3 |2 9 1 |6 8 4 
 I1 I2 I3| I4 I5 I6| I7 I8 I9    1 . 4 |. . . |. . .     1 6 4 |8 7 5 |2 9 3 



                                 7 . . |9 . 2 |. 4 .     7 3 5 |9 1 2 |8 4 6 
                                 . . . |5 . 4 |. . 3     6 1 2 |5 8 4 |9 7 3 
                                 . . 9 |. . . |. 1 .     8 4 9 |3 7 6 |2 1 5 
                                 ------+------+------    ------+------+------
                                 1 7 . |. 9 . |. . .     1 7 3 |6 9 8 |5 2 4 
                                 . . 6 |4 2 1 |7 . .     5 9 6 |4 2 1 |7 3 8 
                                 . . . |. 3 . |. 6 9     2 8 4 |7 3 5 |1 6 9 
                                 ------+------+------    ------+------+------
                                 . 2 . |. . . |6 . .     9 2 8 |1 4 3 |6 5 7 
                                 3 . . |2 . 9 |. . .     3 6 7 |2 5 9 |4 8 1 
                                 . 5 . |8 . 7 |. . 2     4 5 1 |8 6 7 |3 9 2 


Using UTF-8 character encoding, we can produce (see sugen.c):

╔═══╤═══╤═══╦═══╤═══╤═══╦═══╤═══╤═══╗
║ 8 │   │   ║   │   │ 7 ║   │ 6 │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │ 9 ║   │ 5 │ 4 ║   │   │ 1 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │   │   ║   │   │   ║ 3 │   │   ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║ 6 │   │   ║   │ 8 │   ║   │ 9 │ 2 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 9 │ 1 │   ║   │ 3 │   ║   │ 8 │ 5 ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 4 │ 5 │   ║   │ 9 │   ║   │   │ 6 ║
╠═══╪═══╪═══╬═══╪═══╪═══╬═══╪═══╪═══╣
║   │   │ 6 ║   │   │   ║   │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║ 2 │   │   ║ 1 │ 4 │   ║ 6 │   │   ║
╟───┼───┼───╫───┼───┼───╫───┼───┼───╢
║   │ 9 │   ║ 8 │   │   ║   │   │ 3 ║
╚═══╧═══╧═══╩═══╧═══╧═══╩═══╧═══╧═══╝

-}
