module Chelleport.Args where

import Chelleport.Types
import Data.Default (Default (def))

parseArgs :: [String] -> Either String Configuration
parseArgs [] = Right def
parseArgs (arg : args)
  | arg `elem` ["-h", "--help"] = Right $ def {configShowHelp = True}
  | arg `elem` ["-m", "--mode"] = case args of
      [] -> Left "Missing value for mode"
      (mode : rest) -> parseArgs rest >>= updateMode mode
  | otherwise = Left $ "Unrecognized argument: " ++ arg

updateMode :: String -> Configuration -> Either String Configuration
updateMode "hints" cfg = Right cfg {configMode = defaultHintsMode}
updateMode "search" cfg = Right cfg {configMode = defaultSearchMode}
updateMode mode _ = Left $ "Invalid mode: " ++ mode
