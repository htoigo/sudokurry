module Main (main) where

import Sudoku
import PrettySudoku

-- solvePuzzle takes a grid string and generates a list of solutions for
-- that puzzle grid.
solvePuzzle :: String -> [Grid]
solvePuzzle =  solve . parseGrid

-- Parse a grid string, allowing '.' as a synonym for '0' (blank).
parseGrid :: String -> Grid
parseGrid = groupN 9 . map dot2Zero . filter validChar

validChar :: Char -> Bool
validChar c = c `elem` (digits ++ "0.")

dot2Zero :: Char -> Char
dot2Zero '.' = '0'
dot2Zero c   = c

showSolns :: [Grid] -> String
showSolns = concat . numberSolns . map renderGrid

numberSolns :: [String] -> [String]
numberSolns gs = zipWith showSoln gs [1..]

showSoln :: String -> Int -> String
showSoln cs n = "Solution #" ++ show n ++ ":\n" ++ cs ++ "\n"


main :: IO ()
main = do
    putStrLn "Input a puzzle to solve:"
    p <- getLine
    putStr (showSolns (solvePuzzle p))
