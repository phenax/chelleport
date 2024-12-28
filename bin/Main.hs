module Main where

import qualified Chelleport
import Chelleport.Args (Configuration (configShowHelp))
import qualified Chelleport.Args as Args
import qualified System.Environment
import System.Exit (exitFailure)

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case Args.parseArgs args of
    Right config
      | configShowHelp config -> showHelp
      | otherwise -> Chelleport.run config
    Left err -> do
      showHelp
      putStrLn $ "[Error] " ++ err
      exitFailure

showHelp :: IO ()
showHelp =
  putStrLn . unlines $
    [ "Control your mouse with your keyboard",
      "See https://github.com/phenax/chelleport for more information",
      "",
      "Usage: chelleport [FLAGS]",
      "",
      "Flags:",
      "  --help: Help menu. This thing right here",
      "  -m <mode>, --mode <mode>: Run program in mode. (search | hints)"
    ]
