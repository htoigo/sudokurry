module Main (main) where

import System.Environment           ( getArgs, getProgName )
import System.Console.GetOpt        ( OptDescr(Option), ArgDescr(..),
                                      ArgOrder(..), getOpt, usageInfo )
import Text.ParserCombinators.ReadP ( readP_to_S )
--import Text.Regex    ( mkRegex, splitRegex )
--import Text.Regex.TDFA ( (=~) )
import Text.RE.TDFA.String
import System.Exit   ( ExitCode(..), exitWith )
import System.IO     ( stderr, stdout, hPutStrLn )
import Data.List     ( isPrefixOf, sort, nub )
import Data.Maybe    ( isNothing, fromJust )

import Sudoku        ( Grid, GridType(..), gridSize, isGridStr,
                       DiffLevel, diffLevels, grid, grids,
                       solve, solutionIO, puzzleIO )
import Sudoku.PrettyPrint ( renderGrid )
import Data.Version ( showVersion )
import Paths_sudoku ( version )

-- A sudoku program,  demonstrating use of the Sudoku module.

-- Usage: sudoku [-hV] COMMAND [-f FILE | GRIDSTR... | puzzle DIFF | solution]
--        sudoku -h | --help
--        sudoku -V | --version
--        sudoku solve [-f FILE | GRIDSTR...] [-t | --time]
--        sudoku grade [-f FILE | GRIDSTR...]
--        sudoku make  solution | puzzle DIFFICULTY [-t | --time]

-- Exit codes returned to the program's caller (other than zero (success)).

parseErr, argErr :: Int
parseErr = 1              -- a parsing error
argErr   = 2              -- an error in specifying arguments
inputErr = 3              -- choked on some input

data Flag = Help               --  -h, --help
          | Version            --  -V, --version
          | Time               --  -t, --time
          | File String        --  -f FILE, --file=FILE
          deriving (Eq, Ord, Show)

data Command = Solve | Make | Grade
             deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]    (NoArg Help)
      "Print this help message."
  , Option ['V'] ["version"] (NoArg Version)
      "Print version and exit."
  , Option ['t'] ["time"]    (NoArg Time)
      "Time the specified operations and print stats."
  , Option ['f'] ["file"]    (ReqArg File "FILE")
      "Read grids from FILE instead of cmdline or stdin."
  ]

-- 'dieWith n s' writes the given error message s to stderr and terminates with
-- exitWith (ExitFailure n), thus returning the given code n to the caller.
dieWith :: Int -> String -> IO a
dieWith n s = do
    hPutStrLn stderr s
    exitWith (ExitFailure n)

-- 'succeed s' writes the given message s to stderr and terminates with
-- a success exit code.
succeed :: String -> IO a
succeed s = do
    hPutStrLn stderr s
    exitWith ExitSuccess

usageHdr :: String -> String
usageHdr prg = "Usage: " ++ prg ++ " [-hV]"
               ++ " COMMAND [-f FILE | GRIDSTR... | puzzle DIFF | solution]\n"
               ++ "       " ++ prg ++ " solve [-f FILE | GRIDSTR...]\
                                      \ [-t | --time]\n"
               ++ "       " ++ prg ++ " grade [-f FILE | GRIDSTR...]\n"
               ++ "       " ++ prg ++ " make  puzzle DIFFICULTY [-t | --time]\
                                      \ | solution"

versionInfo :: String -> String
versionInfo prg = prg ++ " " ++ showVersion version

-- parseArgs accepts the list of args on the command line (except the program
-- name) and yields an IO triple, containing the command, the list of options
-- and the list of positional command arguments.

parseArgs :: String -> [String] -> IO (Command, [Flag], [String])
parseArgs prg argv =
    case getOpt Permute options argv of
        (opts,posArgs,[])
            | Help `elem` opts ->
                succeed (usageInfo (usageHdr prg) options)
            | Version `elem` opts ->
                succeed (versionInfo prg)
            | otherwise -> do
                (cmd,inps) <- parsePos prg opts posArgs
                return (cmd, sort (nub opts), inps)
        (_,_,errs) ->
            dieWith parseErr (concat errs ++ usageInfo (usageHdr prg) options)

-- 'parsePos' parses the positional arguments.  It accepts the program name, the
-- options list, and the list of positional arguments.  It yields a pair
-- consisting of the desired command (solve, grade, or make) and a list of
-- inputs or arguments to this command.  NOTE: There must be at least one
-- positional argument, as this is the COMMAND.

parsePos :: String -> [Flag] -> [String] -> IO (Command, [String])
parsePos prg _ [] = do
    dieWith argErr ("A COMMAND is required: 'solve', 'grade', or 'make'.\n"
                    ++ usageInfo (usageHdr prg) options)
parsePos prg opts (pa:pas)
    | pa `isPrefixOf` "solve" = parseSolve prg opts pas
                                >>= \inps -> return (Solve, inps)
    | pa `isPrefixOf` "grade" = parseGrade prg opts pas
                                >>= \inps -> return (Grade, inps)
    | pa `isPrefixOf` "make"  = parseMake prg opts pas
                                >>= \puzsol -> return (Make, puzsol)
    | otherwise =
        dieWith argErr ("First positional arg must be a COMMAND:\
                        \ 'solve', 'grade', or 'make'.\n"
                        ++ usageInfo (usageHdr prg) options)

-- parseSolve parses the args for the Solve command.
parseSolve :: String -> [Flag] -> [String] -> IO [String]
parseSolve prg opts pas
    | fileSet = if null pas
                then return []
                else dieWith argErr
                             ("Cannot specify -f FILE option and also grids\
                              \ on the cmdline.\n"
                              ++ usageInfo (usageHdr prg) options)
    | otherwise = if null pas then return ["-"] else return pas
    where fileSet = (File "") `isIn` opts

-- parseGrade parses the args for the Grade command.
parseGrade :: String -> [Flag] -> [String] -> IO [String]
parseGrade prg opts pas
    | Time `isIn` opts =
        dieWith argErr
                ("Cannot specify --time option with Grade command.\n"
                 ++ usageInfo (usageHdr prg) options)
    | otherwise = parseSolve prg opts pas

-- parseMake parses the args for the Make command, either
-- 'puzzle DIFFICULTY [-t|--time]' or 'solution'.
parseMake :: String -> [Flag] -> [String] -> IO [String]
parseMake prg _ [] =
    dieWith argErr ("Make command requires 'puzzle' or 'solution'.\n"
                    ++ usageInfo (usageHdr prg) options)
parseMake prg opts (pa:pas)
    | (File "") `isIn` opts =
        dieWith argErr
                ("Cannot specify -f FILE option with Make command.\n"
                 ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "solution" =
        if Time `isIn` opts
        then dieWith argErr
                     ("Cannot specify --time option when making a solution.\n"
                      ++ usageInfo (usageHdr prg) options)
        else return ["solution"]
    | pa `isPrefixOf` "puzzle"  && null pas =
        dieWith argErr ("Make puzzle command requires a DIFFICULTY level.\n"
                        ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "puzzle" =
        let dstr = head pas
            d    = read dstr :: DiffLevel
        in if d `elem` diffLevels
           then return ["puzzle", dstr]
           else dieWith inputErr "Make puzzle: invalid DIFFICULTY level."
    | otherwise =
        dieWith argErr ("Make command requires 'puzzle' or 'solution'.\n"
                        ++ usageInfo (usageHdr prg) options)

-- Because File s == File t is only true if s == t, we cannot use 'elem' to test
-- whether File is in the list of options.  We need another test of equality,
-- which is true for any two File String flags, that is, File s ~= File t,
-- regardless of what s and t are.

-- (~=) on files does not care about what the actual filename is, but for other
-- flags it is the same as (==).
(~=) :: Flag -> Flag -> Bool
(File _) ~= (File _) = True
a        ~= b        = a == b

-- (/~=) is the negation of (~=).
(/~=) :: Flag -> Flag -> Bool
f /~= g = not (f ~= g)

-- Now we can test if any Flag is in the options list, including
-- `File String's.
notIn :: Flag -> [Flag] -> Bool
notIn =  all . (/~=)

isIn :: Flag -> [Flag] -> Bool
isIn = any . (~=)


main :: IO ()
main = do
    prg <- getProgName;  argv <- getArgs
    (cmd,opts,inps) <- parseArgs prg argv
{-
    putStrLn $ "Command: " ++ show cmd
    putStrLn $ "Options: " ++ show opts
    putStrLn $ "Inputs: " ++ show inps
-}
    case (cmd,opts,inps) of
        (Solve,[],gStrs) ->          -- a non-null list of grid strings on the cmdline
            --putStr (showPuzzleSolutions gStrs)
            putStr $ (showAllSolns' . solveAll' . gridsFromStrs) gStrs

        (Solve,[Time],gStrs) ->      -- a list of strings, each one a grid
            error "Timed solve not implemented yet."

        (Solve,[File f],_) -> do    -- a single string, containing possibly many grids
            inp <- readFile f
            --putStr $ (showPuzzleSolutions . gridStrings) inp
            putStr $ (showAllSolns . solveAll . gridsList) inp

        (Solve,_,["-"]) -> do       -- ditto, but from STDIN
            inp <- getContents
            putStr $ (showAllSolns . solveAll . gridsList) inp

        (Make,_,["solution"]) -> putStrLn . renderGrid =<< solutionIO

        (Make,_,["puzzle", dstr]) ->
            let d = read dstr :: DiffLevel
            in putStrLn . renderGrid =<< puzzleIO d

gridsList :: String -> Maybe [Grid]
gridsList s = gridsFrom possibleParses
    where
      gridsFrom :: [([Grid], String)] -> Maybe [Grid]
      gridsFrom []         = Nothing
      gridsFrom ps = Just (fst (last ps))
      possibleParses :: [([Grid], String)]
      possibleParses = readP_to_S grids s

solveAll :: Maybe [Grid] -> [(Grid, [Grid])]
solveAll Nothing   = []
solveAll (Just gs) = map solve1 gs

solve1 :: Grid -> (Grid, [Grid])
solve1 g = (g, solve g)

showAllSolns :: [(Grid, [Grid])] -> String
showAllSolns []  = "Unable to read puzzles from input.\n\n"
showAllSolns pss = (concat . map show1Solns . zip [1..]) pss

show1Solns :: (Int, (Grid, [Grid])) -> String
show1Solns (n,(p,ss)) = showNthPuzzle n p ++ showNthSolns n ss

showNthPuzzle :: Int -> Grid -> String
showNthPuzzle n g = "Puzzle #" ++ show n ++ ":\n" ++ renderGrid g ++ "\n"

showNthSolns :: Int -> [Grid] -> String
showNthSolns _ [] = "This puzzle has no solutions.\n\n"
showNthSolns n gs = concat . map showSoln . numberSolns $ map renderGrid gs
    where numberSolns gs = zip [1..] gs
          showSoln (m,g) = "P" ++ show n ++ " - "
                           ++ "Solution #" ++ show m ++ ":\n" ++ g ++ "\n"

gridsFromStrs :: [String] -> [Maybe Grid]
gridsFromStrs ss = map gridFrom possibleParses
    where
        gridFrom :: [(Grid, String)] -> Maybe Grid
        gridFrom []        = Nothing
        gridFrom ((g,s):_) = Just g
        possibleParses :: [[(Grid, String)]]
        possibleParses = map (readP_to_S grid) ss

solveAll' :: [Maybe Grid] -> [Maybe (Grid, [Grid])]
solveAll' = map solve1'

solve1' :: Maybe Grid -> Maybe (Grid, [Grid])
solve1' Nothing   = Nothing
solve1' (Just g)  = Just (g, solve g)

showAllSolns' :: [Maybe (Grid, [Grid])] -> String
showAllSolns' = concat . map show1Solns' . zip [1..]

show1Solns' :: (Int, Maybe (Grid, [Grid])) -> String
show1Solns' (n,Nothing) = "Unable to read puzzle #" ++ show n ++ "\n\n"
show1Solns' (n,Just (p,ss)) = showNthPuzzle n p ++ showNthSolns n ss

{-
-- 'gridStrings' accepts a single string containing possibly many grids and
-- returns a list of strings, each one representing a single grid.
gridStrings :: String -> [String]
gridStrings str = case firstLine str of
                    l1 | l1 =~ gridSep -> [""] -- tail (splitRegex gridSep str)
                       | isGridStr l1 -> lines str
    where firstLine = head . lines
          gridSep   = [re|(G|g)rid [0-9]+[[:space:]]*|]

showPuzzleSolutions :: [String] -> String
showPuzzleSolutions = showSolns . puzzleSolns . grids

grids :: [String] -> [[(Grid, String)]]
grids = map (readP_to_S grid)

-- Important for puzzle numbering
puzzleSolns :: [[(Grid, String)]] -> [Maybe (Grid, [Grid])]
puzzleSolns = map solvePuzzle
    where solvePuzzle :: [(Grid, String)] -> Maybe (Grid, [Grid])
          solvePuzzle []        = Nothing
          solvePuzzle ((g,s):_) = Just (g, solve g)

showSolns :: [Maybe (Grid, [Grid])] -> String
showSolns = concat . map show1PuzlSolns . zip [1..]

show1PuzlSolns :: (Int, Maybe (Grid, [Grid])) -> String
show1PuzlSolns (n,mps)
    | isNothing mps = "Unable to read puzzle #" ++ show n ++ "\n\n"
    | otherwise = showNthPuzzle n p ++ showNthSolns n ss
    where p  = fst pss
          ss = snd pss
          pss = fromJust mps
-}
