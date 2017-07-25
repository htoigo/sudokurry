module Sudoku.Random where

-- Control.Monad.State from the 'mtl' package is the same as
-- Control.Monad.State.Lazy
import Control.Monad.State ( State, get, put )
import Control.Monad       ( liftM, forM, replicateM )
import System.Random       ( RandomGen, StdGen, mkStdGen, getStdRandom,
                             Random, random, randomR )
import Data.List           ( sortBy )
import Data.Ord            ( comparing )
import System.CPUTime      ( getCPUTime )
import Data.Time           ( getCurrentTime, UTCTime(..) )
import Data.Ratio          ( numerator, denominator )

import Data.Array.ST
import Control.Monad.ST
import Data.STRef

-- Initialize the StdGen in the State monad used by these functions.  The
-- following algorithm is taken from the System.Random module in the random
-- package.

initStdGen :: IO StdGen
initStdGen = do
  cput <- getCPUTime
  (sec, psec) <- getTime
  return (mkStdGen (fromInteger $ sec * 12345 + psec + cput))

-- getTime yields a pair of Integers (s,ps) where s and ps are the number of
-- seconds and picoseconds, respectively, since midnight for the current UTC.

getTime :: IO (Integer, Integer)
getTime = do
  utc <- getCurrentTime
  let daytm = toRational (utctDayTime utc)
  return $ quotRem (numerator daytm) (denominator daytm)
  
{-
initState :: StdGen -> State StdGen
initState sg = do
  put sg
-}

getRandom :: (Random a) => State StdGen a
getRandom = do
  rg <- get
  let (v, rg') = random rg
  put rg'
  return v

{-
runGetRandom :: (Random a) => IO a
runGetRandom = do
    s <- getStdGen
    let (v, s') = runState getRandom s
    setStdGen s'
    return v
-}

getNRandoms :: (Random a) => Int -> State StdGen [a]
getNRandoms n = replicateM n getRandom

{-
runGetNRandoms :: (Random a) => Int -> IO [a]
runGetNRandoms n = do
    s <- getStdGen
    let (vs, s') = runState (getNRandoms n) s
    setStdGen s'
    return vs
-}

getRandomR :: (Random a) => (a, a) -> State StdGen a
getRandomR (l,h) = do
  rg <- get
  let (v, rg') = randomR (l,h) rg
  put rg'
  return v

getNRandomRs :: (Random a) => Int -> (a, a) -> State StdGen [a]
getNRandomRs n (l,h) = replicateM n (getRandomR (l,h))


-- | Randomly shuffle a list.  Uses the Fisher-Yates algorithm, as
--   implemented by Richard Durstenfeld.
--   /O(N)/

shuffle :: (RandomGen g) => [a] -> g -> ([a], g)
shuffle xs gen = runST ( do
    g <- newSTRef gen
    let randomRST lohi = do
          (a,s') <- liftM (randomR lohi) (readSTRef g)
          writeSTRef g s'
          return a
    ar <- newArray n xs
    xs' <- forM [1..n] $ \i -> do
        j <- randomRST (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        return vj
    gen' <- readSTRef g
    return (xs', gen') )
  where
    n = length xs
    newArray :: Int -> [a] -> ST s (STArray s Int a)
    newArray n xs = newListArray (1,n) xs

-- | shuffleIO wraps shuffle for easier use from within the IO monad,
--   employing the IO monad's global StdGen variable.

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle xs)
