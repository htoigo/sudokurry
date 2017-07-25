module Main (main) where

import System.Random

import Sudoku
import Sudoku.PrettyPrint ( renderGrid )

--import System.Environment (getArgs)

showSolns :: [Grid] -> String
showSolns = concat . numberSolns . map renderGrid

numberSolns :: [String] -> [String]
numberSolns gs = zipWith showSoln gs [1..]

showSoln :: String -> Int -> String
showSoln cs n = "Solution #" ++ show n ++ ":\n" ++ cs ++ "\n"

main :: IO ()
main = do
    putStrLn "Generating a solution grid..."
    rg <- newStdGen
    putStr (showSolns (take 1 $ solutions rg))
