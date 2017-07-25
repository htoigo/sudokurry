module Main (main) where

import System.Random
import Data.Array.IO
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Data.STRef

-- Fisher-Yates shuffle algoritm (modern version, introduced by
-- Richard Durstenfeld in 1964).  /O(N)/
shuffle_FY :: [a] -> IO [a]
shuffle_FY xs = do
    ar <- newArray n xs
    forM [1..n] $ \i -> do
        j <- randomRIO (i,n)
        vi <- readArray ar i
        vj <- readArray ar j
        writeArray ar j vi
        writeArray ar i vj
        return vj
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray n xs = newListArray (1,n) xs

shuffle' :: [a] -> StdGen -> ([a], StdGen)
shuffle' xs gen = runST ( do
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

shuffleIO :: [a] -> IO [a]
shuffleIO xs = getStdRandom (shuffle' xs)


main :: IO ()
main = undefined
--do
--  let n = 1000
--      s = "bird"
--  forM [1..n] $ \i -> do
--    s' <- shuffle_my s
