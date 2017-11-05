{-# LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}
-- The above language pragma is needed for instance Read Grid.

module Sudoku where    -- temporarily export everything for debugging

{-
module Sudoku
  ( Grid, Matrix, Row, Digit,
    gridSize,
    digits, blankC, blank, isGridStr,  --  also export isSudoChar???
    grid, grids                        -- parse a grid, or multiple grids
    solve,
    groupN, ungroup,
    solution, solutionIO, solutions,
    DiffLevel, Difficulty,
    diffLevels,
    difficulty,
    puzzle, puzzleIO,

    GridType(Puzl,Soln),
    startingGrid
  ) where
-}

import Data.Maybe ( fromJust )
import Data.Tuple ( swap )
--import Prelude hiding ( minimum )
import System.Random ( RandomGen, split,
                       randomR, getStdRandom )

import Text.ParserCombinators.ReadP ( ReadP, satisfy, count, skipSpaces, (+++),
                                      char, string, between, choice,
                                      many1, munch1 )
import Control.Monad ( liftM2 )

import Text.ParserCombinators.ReadPrec ( lift )
import Text.Read ( Read(..) )

import Sudoku.Random ( shuffle )

-- The 'order' is a metric of the sudoku grid size, that is, the grid size's
-- order of magnitude.  It equals the length of a side of one of the smaller
-- boxes in the grid.  Thus, the length of a side of the overall grid is the
-- square of the order.  In the usual case, the order is 3, and so the grid
-- size is 3^2 = 9.

gridOrder, gridSize :: Int
gridOrder = 3
gridSize = gridOrder ^ 2

-- A matrix is a list of rows:

type Matrix a = [Row a]
type Row a = [a]

-- A grid is a 9x9 matrix of digits:

type Digit = Char
type Grid = Matrix Digit

-- The valid digits are 1 to 9 with 0 standing for a blank:

digits :: [Digit]
digits = ['1'..'9']

-- 'digits' is in order from least to greatest, so we can give a name to
-- the least digit and the greatest digit.

minDigit :: Digit
minDigit = head digits

maxDigit :: Digit
maxDigit = last digits

-- And '0' stands for a blank cell:

blankC :: Digit
blankC = '0'

blank :: Digit -> Bool
blank = (== '0')


{-- Parsing sudoku grid specifications --}

{- We would like to be able to accept grid specifications of the following
   forms:
[['4','0','0','0','0','0','8','0','5'],['0','3','0','0','0','0','0','0','0'],['0','0','0','7','0','0','0','0','0'],['0','2','0','0','0','0','0','6','0'],['0','0','0','0','8','0','4','0','0'],['0','0','0','0','1','0','0','0','0'],['0','0','0','6','0','3','0','7','0'],['5','0','0','2','0','0','0','0','0'],['1','0','4','0','0','0','0','0','0']]

["400000805","030000000","000700000","020000060","000080400","000010000","000603070","500200000","104000000"]

"4.....8.5.3..........7......2.....6.....8.4......1.......6.3.7.5..2.....1.4......"

"
400000805
030000000
000700000
020000060
000080400
000010000
000603070
500200000
104000000"

TODO:
"
4 . . |. . . |8 . 5 
. 3 . |. . . |. . . 
. . . |7 . . |. . . 
------+------+------
. 2 . |. . . |. 6 . 
. . . |. 8 . |4 . . 
. . . |. 1 . |. . . 
------+------+------
. . . |6 . 3 |. 7 . 
5 . . |2 . . |. . . 
1 . 4 |. . . |. . . 
"
-}

-- A valid character for a cell of a sudoku grid specification is either a
-- digit (1-9) or a blank ('0' or '.').
isSudoChar :: Char -> Bool
isSudoChar c = c `elem` (digits ++ "0.")

isGridStr :: String -> Bool
isGridStr s = (length s == gridSize^2) && (all isSudoChar s)

-- When receiving grids from input, we convert all '.' characters to '0', so
-- that internally a blank cell is always represented by a zero.
dot2Zero :: Char -> Char
dot2Zero '.' = '0'
dot2Zero c   = c

-- 'sudoChar' parses a single cell from a sudo grid spec, yielding either
-- a digit or a '0' or '.'.
sudoChar :: ReadP Char
sudoChar = satisfy isSudoChar

-- Parse a single comma:
comma :: ReadP Char
comma = char ','

-- 'sepByN n p sep' is a fixed-length version of 'sepBy1 p sep'.  It parses
-- exactly n occurrences of p, separated by sep.  Returns the list of values
-- returned by p.
sepByN :: Int -> ReadP a -> ReadP sep -> ReadP [a]
sepByN n p sep = liftM2 (:) p (count (n-1) (sep >> p))

-- 'row' is used by the gridList parser.  It parses a single row as a list
-- of Haskell 'Char's, either in '['3','0','0'...]' form or "300..." form.
row :: ReadP (Row Digit)
row = rowList +++ rowStr'

-- A row as a list of Haskell 'Char's:
rowList :: ReadP (Row Digit)
rowList = fmap (map dot2Zero)
               (between (char '[') (char ']')
                        (sepByN gridSize quotedChar comma))

quotedChar :: ReadP Char
quotedChar = between quote quote sudoChar
    where quote = char '\''

-- "rowStr'" parses the quoted version of a row string:
rowStr' :: ReadP (Row Digit)
rowStr' = do
    _ <- char '"'
    r <- count gridSize sudoChar
    _ <- char '"'
    return (map dot2Zero r)

-- 'gridList' parses a sudoku grid in list form--i.e., '[row1, row2, ...]'.
gridList :: ReadP Grid
gridList = between (char '[') (char ']') (sepByN gridSize row comma)

-- 'rowStr' parses a bare row string:
rowStr :: ReadP (Row Digit)
rowStr = fmap (map dot2Zero) (count gridSize sudoChar)

-- 'gridStr' parses a sudoku grid in string form, either all on one line,
-- or one row per line--i.e.,
-- "300000000050200000...", or
-- "
-- 300000000
-- 000050200
-- 000000800
-- ...
-- ".
gridStr :: ReadP Grid
gridStr = count gridSize (skipSpaces >> rowStr)

{- Parse a list of grid specifications with headings in the form used by Project
   Euler:
   Grid 01
   003020600
   900305001
   001806400
   008102900
   700000008
   006708200
   002609500
   800203009
   005010300
   Grid 02
   200080300
   etc...   -}

-- Parse the literal string 'Grid':
strGrid :: ReadP String
strGrid = string "Grid" +++ string "grid"

-- Parse any single digit (here '0' counts as a digit as well):
digit :: ReadP Char
digit = satisfy isDigit

isDigit :: Char -> Bool
isDigit = flip elem "0123456789"

-- Parse a number:
number :: ReadP String
number = munch1 isDigit

-- Parse a grid title (heading):
title :: ReadP String
title = do
    skipSpaces
    s1 <- strGrid
    skipSpaces
    s2 <- number
    skipSpaces
    -- munch the rest of the line
    -- ...
    return (s1 ++ " " ++ s2)

-- Parse a list of one or more titled grids:
titledGrid :: ReadP Grid
titledGrid = title >> gridStr

-- Parse a grid (in any of the above formats):
grid :: ReadP Grid
grid = choice [gridList, gridStr, titledGrid]

-- 'grids' parses a string into list of one or more grids:
grids :: ReadP [Grid]
grids = many1 grid


{-- Serialization (using read & show instances) --}

-- The following produces the error "Overlapping instances for Read Grid"
--   Matching instances:
--     instance Read a => Read [a] -- Defined in 'GHC.Read'
--     instance Read Grid          -- Defined at src/Sudoku.hs:198:10

--instance Read Grid where
--    readPrec = lift grid


{-- Sudoku solver --}

{-
  We assume the input grid contains only digits and blanks, so we do not have
  to check for the input being well-formed.
-}

{-
-- The first iteration of the solution

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . choices

-- The function choices installs the available digits for each cell:
choices :: Grid -> Matrix [Digit]
choices = map (map choice)
        where choice d = if blank d then digits else [d]

-- Expand takes a matrix each of whose entries is a list of digits and
-- converts this matrix into a list of grids by installing all the choices
-- in all possible ways.
expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp
-}

{-
-- Second iteration of the solution

-- The 'completions' function was merged into 'solve':

solve = filter valid . expand . choices

-- To optimize 'solve', we prune the choices before expanding:

solve :: Grid -> [Grid]
solve = filter valid . expand . many prune . choices
-}

-- Third version of solve:
solve :: Grid -> [Grid]
solve = search expand1 . choices

-- search takes expand function (which expands one cell of the matrix)
-- and a matrix of choices, ...
search expandFn cm
    | not (safe pm) = []
    | complete pm   = [extract pm]
    | otherwise     = concat (map (search expandFn) (expandFn pm))
    where pm = prune cm
-- We could replace 'prune' in the last line, above, by 'many prune'.
-- Sometimes many prunes work faster than one prune; sometimes not.

-- The function choices installs the available digits for each cell:
choices :: Grid -> Matrix [Digit]
choices = map (map choice)
        where choice d = if blank d then digits else [d]

many :: (Eq a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
         where y = f x

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows
    where pruneBy f = f . map pruneRow . f

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
    where fixed = [d | [d] <- row]

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

-- Expand takes a matrix each of whose entries is a list of digits and
-- converts this matrix into a list of grids by installing all the choices
-- in all possible ways.
expand :: Matrix [Digit] -> [Grid]
expand = cp . map cp

-- Expanding a single cell
-- Make use of a partial expansion that installs the choices for just one of
-- the cells, and start the pruning process again on each result. Hopefully,
-- this will lead to a solution more quickly.

-- Expand1 is a partial function that expands the choices for one cell only.
-- This function will return well-defined results only for incomplete matrices,
-- and on such matrices is required to satisfy
--     expand = concat . map expand . expand1,
-- up to some permutation of the answers.

{-
The first definition of expand1, which finds the first cell in the
matrix with a non-singleton entry and uses that for expansion:

expand1 rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where (rows1,row:rows2) = break (any (not . single)) rows
          (row1,cs:row2)    = break (not . single) row

-}

-- The problem with the above definition of expand1 is that it can
-- lead to wasted work.  If the first non-singleton entry found
-- happens to be the empty list, then expand1 will return the empty
-- list (no solutions). But if such a list is buried deep in the
-- matrix, then expand1 will do a lot of useless calculation trying to
-- find a solution that isn't there.

-- A better choice of cell on which to perform expansion is one with
-- the fewest number of choices (not equal to 1). A cell with no
-- choices means that the puzzle is unsolvable, so identifying such a
-- cell quickly is a good idea.

expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs]
    where (rows1,row:rows2) = break (any smallest) rows
          (row1,cs:row2)    = break smallest row
          smallest cs       = length cs == n
          n                 = minimum (counts rows)

counts :: Matrix [Digit] -> [Int]
counts = filter (/= 1) . map length . concat

{-
minimum :: [Int] -> Int
minimum ns = foldl1 min ns
-}

complete :: Matrix [Digit] -> Bool
complete = all (all single)

single :: [a] -> Bool
single [_] = True
single _   = False

safe :: Matrix [Digit] -> Bool
safe cm = all ok (rows cm) &&
          all ok (cols cm) &&
          all ok (boxs cm)

ok row = nodups [x | [x] <- row]

extract :: Matrix [Digit] -> Grid
extract = map (map head)

{-- We need a function that takes, in all possible ways, one element from the
    first list, one element from the second list and one element from the third
    list. Let's call the function that does this 'cp', short for `cartesian
    product'.

    To come up with this function, we assume
      cp [[2],[1,3]] == [[2,1],[2,3]]
    Then we extend this definition to one for
      cp ([1,2,3]:[[2],[1,3]]) == [x:ys | x <- xs, ys <- cp xss]
    But a more efficient definition is the following, which guarantees
    that cp xss is computed just once:
      cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
                    where yss = cp xss             --}

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- yss]
              where yss = cp xss

-- 'valid g' is True iff the grid g satisfies the three game constraints of
-- having no duplicate digits in any row, column, or box.

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
          all nodups (cols g) &&
          all nodups (boxs g)

-- We must be careful not to count multiple zeroes as duplicates, since
-- they represent blank cells.  To accomplish this, we define a new 'notEq'
-- operator, which treats zeroes as not equal to one another.

nodups :: [Digit] -> Bool
nodups []     = True
nodups (x:xs) = all (`notEq` x) xs && nodups xs

notEq :: Digit -> Digit -> Bool
notEq d1 d2
  | blank d1 || blank d2 = True
  | otherwise            = d1 /= d2

{-- The functions rows, cols and boxs take a matrix to a list of its rows,
    columns or boxes, respectively.  --}

-- Since a matrix is given by a list of its rows, the rows function is just
-- the identity function on matrices:
rows :: Matrix a -> Matrix a
rows = id

-- Function cols computes the transpose of a matrix.
cols :: Matrix a -> Matrix a
cols [xs] = [[x] | x <- xs]
cols (xs:xss) = zipWith (:) xs (cols xss)

-- The function boxs takes a matrix and returns a matrix whose rows
-- are the boxes of the original matrix.
boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
       map cols .
       group . map group

-- The function group splits a list into groups of three:
group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

-- groupN takes an integer n and a list xs and returns a list of lists such that
-- the concatenation of the result is equal to xs.  Furthermore, each sublist
-- has length n.  If n >= length xs, then groupN returns [xs].

groupN :: Int -> [a] -> [[a]]
groupN n xs  | n <= 0 = [xs]
groupN _ []           = []
groupN n xs           = take n xs : groupN n (drop n xs)

-- Function ungroup takes a grouped list and ungroups it:
ungroup :: [[a]] -> [a]
ungroup = concat


{-- Sudoku solved grid generator --}

-- GridType is used for specifying which type of grid to generate, a puzzle
-- or a solved grid.

data GridType = Puzl | Soln
              deriving (Eq, Show)

-- Generate a random solved sudoku grid--that is, a valid completed grid.

-- A list of solutions can be generated from an empty grid by the solver,
-- although it always produces solutions in the same order.  We can randomize
-- the order of solutions produced by modifying the expand1 function, which
-- installs the choices for a single cell, so that it does so in non-
-- deterministic order.  This variation is called 'expand1Rnd', and the new
-- version of 'solve' which uses 'expand1Rnd' we call 'solveRnd'.

solveRnd :: (RandomGen g) => g -> Grid -> ([Grid], g)
solveRnd rg grid = (solns, rg2)
  where solns     = search (expand1Rnd rg1) (choices grid)
        (rg1,rg2) = split rg

expand1Rnd :: (RandomGen g) => g -> Matrix [Digit] -> [Matrix [Digit]]
expand1Rnd rg rows = [rows1 ++ [row1 ++ [c]:row2] ++ rows2 | c <- cs']
  where (rows1,row:rows2) = break (any smallest) rows
        (row1,cs:row2)    = break smallest row
        smallest cs       = length cs == n
        n                 = minimum (counts rows)
        (cs',rg')         = shuffle cs rg


-- This mixing of the order of the solutions produced from an empty grid still
-- is rather lacking in variation.  So, to enhance the variety of generated
-- puzzles, we start with a randomized starting grid in which n cells chosen at
-- random are filled with random digits while satisfying the three game
-- constraints.

-- Following the practice of wholemeal programming, rather than employing row
-- and column indices to position the n randomized cells, we simply produce a
-- list of n random digits via 'randomDigits n rg' and append to it enough
-- blanks to make a complete grid.  This 'gridStr' is then shuffled into random
-- order and grouped into rows of the proper length, to produce a tentative
-- grid:

tentativeGrid :: (RandomGen g) => Int -> g -> (Grid, g)
tentativeGrid n rg = (groupN gridSize gridStr, rg3)
  where
    (gridStr,rg3) = shuffle (ds ++ replicate (gridSize^2 - n) blankC) rg1
    ds            = randomDigits n rg2
    (rg1,rg2)     = split rg

-- randomDigits takes an integer and a random number generator, and produces
-- a list of n random digits.  Each digit is produced by 'randomR (minDigit,
-- maxDigit) rg' and it is consed onto the remainder of the list, generated
-- by recursively calling randomDigits with (n-1).

randomDigits :: (RandomGen g) => Int -> g -> [Digit]
randomDigits 0 _   = []
randomDigits n rg = d : randomDigits (n-1) rg'
  where (d,rg') = randomR (minDigit, maxDigit) rg

-- Finally, to ensure the three game constraints are satisfied, we test our
-- tentative grid using the 'valid' function.  If it is valid, then we have our
-- starting grid.  If it is not, then we recursively call the 'startingGrid'
-- function, repeating the process until we have a valid grid--i.e., one for
-- which the game constraints hold.

startingGrid :: (RandomGen g) => Int -> g -> (Grid, g)
startingGrid n rg =
  if valid tg then (tg,rg') else startingGrid n rg'
  where (tg,rg') = tentativeGrid n rg

{- We no longer need a definition of an empty grid.  'emptyGrid' was used in a
   tentative definiton of startingGrid, in which n cell locations chosen at
   random from an empty grid were filled with random digits, but we changed to a
   wholemeal style of solving this problem.

-- An empty grid consists of 'gridSize' empty rows:

emptyGrid :: Grid
emptyGrid = replicate gridSize emptyRow

-- And an empty row is simply a list of 'gridSize' blank cells:

emptyRow :: Row Digit
emptyRow = replicate gridSize blankC
-}

-- Now, our randomized list of solutions can be defined by taking the randomized
-- starting grid, with 'initialFixed' cells fixed with random digits, and
-- solving it with the randomized version of the solver, solveRnd.  This
-- produces a list of all the valid solutions to the starting grid, each one a
-- complete and valid solution grid, in random order.

initialFixed = 12

solutions :: (RandomGen g) => g -> ([Grid], g)
solutions rg = solveRnd rg' grid
  where (grid,rg') = startingGrid initialFixed rg

-- We then return the head of the 'solutions' list as our generated solution
-- grid:

solution :: (RandomGen g) => g -> (Grid, g)
solution rg = (head gs, rg')
  where (gs,rg') = solutions rg

-- If one is using the IO monad's hidden StdGen variable, there is a wrapped
-- version:
solutionIO :: IO Grid
solutionIO = getStdRandom solution


{-- Statistical analysis --}

-- Compute for different values of n the probability that a grid with n givens
-- is solvable and how long it takes.

-- Fix n, then perform 10000 trials.  Each trial consists of running the solver
-- to see if a complete solution is generated and how long it takes.  (How long
-- is reasonable?)  Construct a histogram of time taken to generate a solution
-- versus n.

-- Use these statistics to fix a value for n,
-- balancing diversity of puzzles generated with number of tries will be needed
-- to generate a puzzle.

-- Construct a histogram showing the distribution of difficulty scores for 500
-- random puzzles generated by 200 iterations each.  Each bin is a range of
-- difficulty scores (0-99, 100-199, 200-299, ... 900-999) and the bar heights
-- are number of puzzles generated in that difficulty range.  See
-- dlbeer.co.nz/articles/sudoku.html.

-- Also see the statistical analysis on Peter Norvig's sudoku page.


{-- Sudoku puzzle difficulty grader --}

-- Puzzle difficulty level is graded on a scale of 1 to 5, with the following
-- meanings:
--   1    Beginner
--   2    Easy
--   3    Medium
--   4    Difficult
--   5    Fiendish

-- NOTE: ast-sudoku (by Glenn Fowler) uses a difficulty rating from 1 (very
-- easy) to 99999 (very difficult).  The 90000-99999 range is exponential.

type DiffLevel = Int

diffLevels :: [DiffLevel]
diffLevels = [1..5]

-- We employ four metrics of difficulty in estimating the difficulty of puzzles:
--   1. Total number of givens.
--   2. Minimum number of givens per row, column, or box.
--   3. Logic techniques that can aid a person in solving.
--   4. Brute force search/solve times by computer (or better, use the number of
--      choices (potential combinations) in solving. i.e., a measure of how many
--      trials a player has to explore if he does not know any logic technique.

-- For each metric, a puzzle is given a difficulty level from 1 to 5.

-- For metric 1, each difficulty level is associated with a range of values for
-- the total number of givens in the puzzle, according to the following
-- association list:

totGivensDiffLvls :: [(DiffLevel, [Int])]
totGivensDiffLvls = [ (1, [50..81])
                    , (2, [36..49])
                    , (3, [32..35])
                    , (4, [28..31])
                    , (5, [22..27]) ]

-- For metric 2, each difficulty level is associated with a lower bound on the
-- number of givens per row, column, or box, according to the following list:

minGivensDiffLvls :: [(DiffLevel, Int)]
minGivensDiffLvls = [ (1, 5)
                    , (2, 4)
                    , (3, 3)
                    , (4, 2)
                    , (5, 0) ]

-- The overall puzzle difficulty is a weighted average of the four metrics:

-- Metric                     Weight    Example
-- Total givens                 0.4        4
-- Min givens / set             0.2        3
-- Req'd logic technique(s)     0.2        2
-- Brute force trials needed    0.2        3
-----------------------------------------------
-- Total:                       1.0       3.2 = 4*0.4 + 3*0.2 + 2*0.2 + 3*0.2

type Difficulty = Float

totGivensWt, minGivensWt, techniquesWt, combinationsWt :: Float
totGivensWt      = 0.4
minGivensWt      = 0.2
techniquesWt     = 0.2
combinationsWt   = 0.2

-- difficulty takes a puzzle grid g and yields its difficulty, as described
-- above.

difficulty :: Grid -> Difficulty
difficulty g = fromIntegral (totGivensDiff g) * totGivensWt
               + fromIntegral (minGivensDiff g) * minGivensWt
               + fromIntegral (techniquesDiff g) * techniquesWt
               + fromIntegral (combinationsDiff g) * combinationsWt

-- Metric 1: Total number of givens in the puzzle.

-- totGivensDiff takes a puzzle grid g and returns its difficulty level based on
-- the total number of givens difficulty levels, defined above.

totGivensDiff :: Grid -> DiffLevel
totGivensDiff g = fromJust $ lookupR (numGivens g) (map swap totGivensDiffLvls)

-- lookupR is a variation of the Prelude function lookup, in which the keys
-- in the association list are ranges instead of single values.
-- lookupR key alist looks for the given key in the lists that are the first
-- element of each pair in the alist.

lookupR :: (Eq a) => a -> [([a],b)] -> Maybe b
lookupR key [] = Nothing
lookupR key ((ks,v):ksvs)
    | key `elem` ks  = Just v
    | otherwise = lookupR key ksvs

-- numGivens takes a puzzle grid and returns the total number of givens in the
-- puzzle.
numGivens :: Grid -> Int
numGivens = length . filter (not . blank) . ungroup

-- Metric 2: The minimum number of givens per row, column, or box.

-- minGivensDiff takes a puzzle grid g and returns its difficulty level based on
-- the minimun number of givens per row, column, or box, according to levels
-- defined above.  This will throw an error if lookupLB returns Nothing.
-- lookupLB should never do that, since the last pair in the alist
-- minGivenDiffLvls has 0 for its lower bound.

minGivensDiff :: Grid -> DiffLevel
minGivensDiff g = fromJust (lookupLB mg minGivensDiffLvls)
                  where mg = minGivensRCB g

-- lookupLB x al takes a value x and the association list that pairs a
-- difficulty level with the minimum number of givens per row, column or box.
-- (See minGivensDiffLvls alist, above.)  It assumes that the lower bounds on
-- givens are monotonically decreasing as one goes through the list from
-- beginning to end.  It finds the first lower bound that is less than or equal
-- to the minimum givens in the puzzle, and returns the difficulty level of that
-- lower bound.

lookupLB :: (Ord b) => b -> [(a,b)] -> Maybe a
lookupLB x [] = Nothing
lookupLB x ((k,v):kvs)
    | x >= v  = Just k
    | otherwise = lookupLB x kvs

-- minGivensRCB determines the minimum number of givens per row, column or box
-- in a given puzzle grid.

minGivensRCB :: Grid -> Int
minGivensRCB g = minimum $
                 map ($ g) [minGivensIn rows, minGivensIn cols, minGivensIn boxs]
                 where minGivensIn f = minGivensRow . f

-- minGivensRow takes a puzzle grid and returns the smallest number of givens
-- per row.
minGivensRow :: Grid -> Int
minGivensRow = minimum . map givensInRow
               where givensInRow = length . filter (not . blank)

-- Metric 3: Logic techniques that can aid in solving.

techniquesDiff :: Grid -> DiffLevel
techniquesDiff g = 0

-- Metric 4:
--   4. Brute force search/solve times by computer (or better, use the number of
--      choices (potential combinations) in solving. i.e., a measure of how many
--      trials a player has to explore if he does not know any logic technique.

combinationsDiff :: Grid -> DiffLevel
combinationsDiff g = 0


{-- Sudoku puzzle generator --}

-- Generate a new puzzle, by first generating a completed solution grid, then
-- emptying cells while verifying that the puzzle has only the one solution
-- until the number of givens or the target difficulty is reached.

-- Exploring the combinatoric space of puzzles that have that grid as their solution.

-- 'puzzle'  takes a target difficulty level and returns a random puzzle of the
-- given difficulty.
-- puzzle :: (RandomGen g) => DiffLevel -> g -> (Grid, g)
-- puzzle d rg = (head ps, rg')
--     where (ps,rg') = puzzles d rg

puzzle :: (RandomGen g) => g -> (Grid, g)
puzzle rg = (foldl clearCellIfOK grid order, rg3)
  where (grid,rg3)  = solution rg1
        (order,_) = shuffle [0..gridSize^2-1] rg2
        (rg1,rg2) = split rg

-- | Take a grid and an integer representing a position within the grid, where
-- positions are numbered left-to-right, top-to-bottom from 0 to 80, and if
-- the cell at that position is not blank and can be cleared without
-- invalidating the puzzle (because it would not be uniquely solvable) then
-- clear that cell, otherwise return the input grid as-is.
clearCellIfOK :: Grid -> Int -> Grid
clearCellIfOK g p
    | blank c   = g
    | otherwise = if has1Soln g' then g' else g
    where c = gStr !! p
          g' = groupN gridSize (clearCellN p gStr)
          gStr = ungroup g

-- | Clear the cell at a given position within a grid string.  The position is
-- given as an integer index into the list of digits, starting from 0.
clearCellN :: Int -> [Digit] -> [Digit]
clearCellN n ds = ds1 ++ blankC : tail ds2
    where (ds1,ds2) = splitAt n ds

-- | Generate a puzzle within the IO monad.  A convenience function for those
-- using the IO monad's hidden StdGen variable.

-- Was:  puzzleIO :: DiffLevel -> IO Grid

puzzleIO :: IO Grid
puzzleIO = getStdRandom (puzzle)


-- 'puzzles' is a list of puzzle grids in which the first grid is a valid
-- solution and each subsequent grid in the list is obtained from the preceding
-- one by blanking out one cell at random,
-- TODO: while ensuring that the game constraints are satisfied?

-- To generate the list, it begins with a random solution grid and uses
--   iterate (hide1 g1) (solution g2)

-- to produce an INFINITE (not) list of grids, where each grid is obtained from the
-- previous one by blanking out one cell at random.  This list is then filtered
-- for grids that are uniquely solvable (by has1Soln), that have a total number
-- of givens in the range corresponding to the desired difficulty level, whose
-- minimum number of givens per row, column or box is above the lower bound for
-- the desired difficulty, and whose overall difficulty matches the desired
-- difficulty.

puzzles :: (RandomGen g) => DiffLevel -> g -> ([Grid], g)
puzzles d rg = (filter (has1Soln .&&. (totGivensInRange d)
                      .&&. (minGivensAboveLB d) .&&. (hasDifficulty d))
              $ iterate (hide1 rg') grid,       rg')
              where (grid,rg') = solution rg

-- .&&. is a combinator allowing chaining of predicates
(.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(.&&.) f g a = (f a) && (g a)

-- 'hide1 g' takes a puzzle grid and empties one cell (for now, at random, but
-- later we may want to allow for choosing among different sequences of cells to
-- empty).

hide1 :: (RandomGen g) => g -> Grid -> Grid
hide1 g = groupN gridSize . blank1Cell g . ungroup

-- 'blank1Cell g' takes a grid string ds (i.e., a list of digits for an entire
-- grid) and blanks out one non-blank digit at random.  Here, p is a random
-- position in the string, and blankC represents a blank cell (see top of file).

blank1Cell :: RandomGen g => g -> [Digit] -> [Digit]
blank1Cell rg ds = if (not . blank) (ds !! p)
                   then ds1 ++ blankC : tail ds2
                   else blank1Cell rg' ds
                   where (ds1, ds2) = splitAt p ds
                         (p, rg') = randomR (0, length ds - 1) rg

-- has1Soln determines whether a puzzle grid is uniquely solvable.
has1Soln :: Grid -> Bool
has1Soln = singleton . solve

singleton :: [a] -> Bool
singleton [x] = True
singleton _   = False

-- The predicates totGivensInRange, minGivensAboveLB and hasDificulty take an
-- integral target difficulty level and a puzzle grid and return True iff the
-- given puzzle satisfies the corresponding difficulty constraint for the given
-- desired difficulty level.

-- totGivensInRange determines whether the given grid's total number of givens
-- is in the range corresponding to the given difficulty level.  This function
-- will throw an error if lookup returns Nothing.

totGivensInRange :: DiffLevel -> Grid -> Bool
totGivensInRange d g = numGivens g `elem` range
                       where range = fromJust (lookup d totGivensDiffLvls)

-- The predicate minGivensAboveLB determines whether the given grid's minimum
-- number of givens per row, column or box is above lb, the lower bound for the
-- given difficulty level.  This function will throw an error if lookup returns
-- Nothing.

minGivensAboveLB :: DiffLevel -> Grid -> Bool
minGivensAboveLB d g = minGivensRCB g >= lb
                       where lb = fromJust (lookup d minGivensDiffLvls)

-- minGivensAboveLB d g =
--     (minGivensRCB g) >= (fromJust $ lookup d minGivensDiffLvls)


-- hasDifficulty takes a target difficulty level td and a puzzle grid and
-- returns True iff the given puzzle's difficulty d is close to the target
-- difficulty.  Close means that round d = td.

hasDifficulty :: DiffLevel -> Grid -> Bool
hasDifficulty td g = round d == td where d = difficulty g
