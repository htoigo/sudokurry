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
import Data.List     ( isPrefixOf, sort, nub, (\\) )
import Data.Maybe    ( isNothing, fromJust )

import Sudoku        ( Grid, GridType(..), gridSize, isGridStr,
                       DiffLevel, diffLevels, grid, grids,
                       solve, solutionIO, puzzleIO )
import Sudoku.PrettyPrint ( renderGrid, Encoding(..) )
import Data.Version ( showVersion )
import Paths_sudoku ( version )

-- A sudoku command line program,  demonstrating use of the Sudoku module.

-- Usage:
--   sudoku -h | --help
--   sudoku -V | --version
--   sudoku [-u | -a] solve [-f FILE | GRIDSTR...] [-t | --time]
--   sudoku [-u | -a] grade [-f FILE | GRIDSTR...]
--   sudoku [-u | -a] make solution
--                       | puzzle [-d DIFF | --difficulty DIFF]
--                                [-c N | --clues N]
--                                [-t | --time]

usageHdr :: String -> String
usageHdr prg = "Usage:\n"
               ++ "  " ++ prg ++ " -h | --help\n"
               ++ "  " ++ prg ++ " -V | --version\n"
               ++ "  " ++ prg ++ " [-u | -a] solve [-f FILE | GRIDSTR...] [-t | --time]\n"
               ++ "  " ++ prg ++ " [-u | -a] grade [-f FILE | GRIDSTR...]\n"
               ++ "  " ++ prg ++ " [-u | -a] make solution\n"
               ++ "  " ++ spc ++ "              | puzzle [-d DIFF | --difficulty DIFF]\n"
               ++ "  " ++ spc ++ "                       [-c N | --clues N]\n"
               ++ "  " ++ spc ++ "                       [-t | --time]"
  where spc = map (const ' ') prg

data Flag = Help                 --  -h, --help
          | Version              --  -V, --version
          | Time                 --  -t, --time
          | AsciiEnc             --  -a, --ascii
          | Utf8Enc              --  -u, --utf8
          | File String          --  -f FILE, --file=FILE
          | Difficulty Int       --  -d DIFF, --difficulty=DIFF
          | Clues Int            --  -c N, --clues=N
          deriving (Eq, Ord, Show)

data Command = Solve | Make | Grade
             deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['h'] ["help"]       (NoArg Help)
      "Print this help message."
  , Option ['V'] ["version"]    (NoArg Version)
      "Print version and exit."
  , Option ['t'] ["time"]       (NoArg Time)
      "Time the specified operations and print stats."
  , Option ['a'] ["ascii"]      (NoArg AsciiEnc)
      "Print output grids using ascii encoding."
  , Option ['u'] ["utf8"]       (NoArg Utf8Enc)
      "Print output grids using prettier unicode characters."
  , Option ['f'] ["file"]       (ReqArg File "FILE")
      "Read grids from FILE instead of cmdline or stdin."
  , Option ['d'] ["difficulty"] (ReqArg (Difficulty . read) "DIFF")
      "The difficulty level of the puzzle to be made."
  , Option ['c'] ["clues"]     (ReqArg (Clues . read) "N")
      "The number of clues you want the puzzle to have."
  ]

versionInfo :: String -> String
versionInfo prg = prg ++ " " ++ showVersion version

-- Exit codes returned to the program's caller (other than zero (success)).

parseErr, argErr, inputErr :: Int
parseErr = 1              -- a parsing error
argErr   = 2              -- an error in specifying arguments
inputErr = 3              -- choked on some input

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

-- parseArgs accepts the program name and the list of args on the command line
-- and yields an IO triple, containing the command, the list of options and the
-- list of positional command arguments.

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
--   'make solution'
-- or
--   'make puzzle [-d DIFF ] [-c N] [-t|--time]'.
parseMake :: String -> [Flag] -> [String] -> IO [String]
parseMake prg _ [] =
    dieWith argErr ("Make command requires 'puzzle' or 'solution'.\n"
                    ++ usageInfo (usageHdr prg) options)
parseMake prg opts (pa:pas)
    | (File "") `isIn` opts =
        dieWith argErr
                ("Cannot specify -f FILE option with Make command.\n"
                 ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "solution" && Time `isIn` opts =
        dieWith argErr
                ("Cannot specify --time option when making a solution.\n"
                 ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "solution" && (Difficulty 0) `isIn` opts =
        dieWith argErr
                ("Cannot specify --difficulty option when making a solution.\n"
                 ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "solution" && (Clues 0) `isIn` opts =
        dieWith argErr
                ("Cannot specify --clues option when making a solution.\n"
                 ++ usageInfo (usageHdr prg) options)
    | pa `isPrefixOf` "solution" = return ["solution"]
    | pa `isPrefixOf` "puzzle" = return ["puzzle"]
    | otherwise =
        dieWith argErr ("Make command requires 'puzzle' or 'solution'.\n"
                        ++ usageInfo (usageHdr prg) options)

-- Because File s == File t is only true if s == t, we cannot use 'elem' to test
-- whether File is in the list of options.  We need another test of equality,
-- which is true for any two File String flags, that is, File s ~= File t,
-- regardless of what s and t are.

-- The same applies to the flags Difficulty n and Clues m.

-- (~=) on files does not care about what the actual filename is, for the flags
-- Difficulty and Clues it does not care about the difficulty rating or number
-- of clues.  But for other flags it is the same as (==).
(~=) :: Flag -> Flag -> Bool
(File _)       ~= (File _)       = True
(Difficulty _) ~= (Difficulty _) = True
(Clues _)     ~= (Clues _)     = True
a              ~= b              = a == b

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
    
    putStrLn $ "Command: " ++ show cmd
    putStrLn $ "Options: " ++ show opts
    putStrLn $ "Inputs: " ++ show inps

    case (cmd,opts,inps) of
        (Solve,os,gStrs)        -- a non-null list of grid strings on the cmdline
            | null (os \\ [AsciiEnc,Utf8Enc])  ->   
                --putStr (showPuzzleSolutions gStrs)
                putStr $ (showAllSolns' (enc os) . solveAll' . gridsFromStrs) gStrs

        (Solve,[Time],gStrs) ->      -- a list of strings, each one a grid
            error "Timed solve not implemented yet."

        (Solve,os@[File f],_) -> do    -- a single string, containing possibly many grids
            inp <- readFile f
            --putStr $ (showPuzzleSolutions . gridStrings) inp
            putStr $ (showAllSolns (enc os) . solveAll . gridsList) inp

        (Solve,os,["-"]) -> do       -- ditto, but from STDIN
            inp <- getContents
            putStr $ (showAllSolns (enc os) . solveAll . gridsList) inp

        (Make,os,["solution"]) -> putStrLn . renderGrid (enc os) =<< solutionIO

        (Make,os,["puzzle"]) -> putStrLn . renderGrid (enc os) =<< puzzleIO (cw os)

-- | 'enc' fs extracts the encoding from a list of flags.  If both utf8 and ascii
--   are specified, utf8 overrides ascii.  If no encoding flag is in the list of
--   flags, default to ascii.
enc :: [Flag] -> Encoding
enc fs
  | Utf8Enc `isIn` fs  = Utf8
  | otherwise          = Ascii

-- | 'cw' extracts the number of clues wanted from a list of flags.  If the Clues
-- flag is not in the list, it yields Nothing.
cw :: [Flag] -> Maybe Int
cw []           = Nothing
cw (Clues n:_)  = Just n
cw (_:fs)       = cw fs

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

showAllSolns :: Encoding -> [(Grid, [Grid])] -> String
showAllSolns _ []  = "Unable to read puzzles from input.\n\n"
showAllSolns e pss = (concat . map (show1Solns e) . zip [1..]) pss

show1Solns :: Encoding -> (Int, (Grid, [Grid])) -> String
show1Solns e (n,(p,ss)) = showNthPuzzle e n p ++ showNthSolns e n ss

showNthPuzzle :: Encoding -> Int -> Grid -> String
showNthPuzzle e n g = "Puzzle #" ++ show n ++ ":\n" ++ renderGrid e g ++ "\n"

showNthSolns :: Encoding -> Int -> [Grid] -> String
showNthSolns _ _ [] = "This puzzle has no solutions.\n\n"
showNthSolns e n gs = concat . map showSoln . numberSolns $ map (renderGrid e) gs
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

showAllSolns' :: Encoding -> [Maybe (Grid, [Grid])] -> String
showAllSolns' e = concat . map (show1Solns' e) . zip [1..]

show1Solns' :: Encoding -> (Int, Maybe (Grid, [Grid])) -> String
show1Solns' _ (n,Nothing) = "Unable to read puzzle #" ++ show n ++ "\n\n"
show1Solns' e (n,Just (p,ss)) = showNthPuzzle e n p ++ showNthSolns e n ss

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
