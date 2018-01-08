{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Random     ( StdGen, newStdGen, split )
import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.Monoid
import Data.IORef
import qualified Data.Text as T

import Sudoku
import Sudoku.PrettyPrint

data MySession = EmptySession
data MyAppState = DummyAppState (IORef StdGen)

-- A web application,  demonstrating another use of the Sudoku module.

-- How to make this a RESTful API?

-- Usage:
-- Run the program.  In your favorite web browser, open:
-- http://localhost:8080/sudoku/make.

main :: IO ()
main =
  do rg <- newStdGen
     ref <- newIORef rg
     spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
     runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app =
  do get root $
         text "What are you looking for?"
     get ("sudoku" <//> "make") $
         do (DummyAppState ref) <- getState
            rg <- liftIO $ atomicModifyIORef' ref split
            text (T.pack (renderGrid Utf8 (fst (puzzle Nothing rg))))

{-     get ("sudoku" <//> "example" <//> var) $ \name ->
       do (DummyAppState rg) <- getState
          visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i+1, i+1)
          text (name <> ", you are visitor number " <> T.pack (show visitorNumber))
 -}
