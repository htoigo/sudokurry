module Sudoku.PrettyPrint
  ( renderGrid
  ) where

import Data.List (intercalate)

import Sudoku (Grid(..), Matrix(..), Row(..), Digit,
               digits, blank, groupN)

-- A first pass at rendering a grid.  Here we render directly to a string, but
-- later we'll render to a Doc.

-- TODO: Import the HughesPJ pretty printer (package 'pretty') for the 'Doc'
-- type.
-- import Text.PrettyPrint (Doc)

-- See: Hughes J., "The Design of a Pretty-printing Library",
--      Wadler P., "A prettier printer"

-- Grid = [Row Digit] = [[Digit]]
-- Row Digit = [Digit]


renderGrid :: Grid -> String
renderGrid = concat . insertEvery 3 hline . map renderRow

renderRow :: Row Digit -> String
renderRow = (++"\n") . concat . insertEvery 3 "|" . map renderDigit

renderDigit :: Digit -> String
renderDigit d
    | blank d         = ". "
    | d `elem` digits = [d, ' ']
    | otherwise       = ""

hline :: String
hline = "------+------+------\n"

-- insertEvery n sep, applied to a list xs, inserts the element
-- sep into xs after every n items.

insertEvery :: Int -> a -> [a] -> [a]
insertEvery n sep  = intercalate [sep] . groupN n


-- renderCMatrix renders a choice matrix--i.e., a matrix of lists of digits.
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
