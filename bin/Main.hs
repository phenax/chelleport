module Main where

import qualified Chelleport
import Data.Text (Text, splitOn)
import qualified Data.Text as Text
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
  putStrLn "Wow"
  Chelleport.open
