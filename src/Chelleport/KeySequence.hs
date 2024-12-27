module Chelleport.KeySequence where

import Chelleport.Types (KeyGrid, KeySequence)
import Chelleport.Utils (findWithIndex, uniq)
import Control.Monad (guard)
import Data.List (elemIndex, isPrefixOf)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified SDL

nextChars :: KeySequence -> KeyGrid -> Maybe [Char]
nextChars keySequence cells =
  case matches of
    [] -> Nothing
    _ -> Just nextCharactersInSequence
  where
    nextCharactersInSequence = uniq $ concatMap (take 1 . drop (length keySequence)) matches
    matches = concatMap (filter $ isPrefixOf keySequence) cells

findMatchPosition :: KeySequence -> KeyGrid -> Maybe (Int, Int)
findMatchPosition keySequence = findWithIndex searchRows 0
  where
    searchRows = fmap fst . findWithIndex searchInRow 0
    searchInRow = guard . (== keySequence)

isAlphabetic :: SDL.Keycode -> Bool
isAlphabetic = (`Map.member` keycodeCharMapping)

-- Linear Congruential Generator
lcg :: Int -> Int
lcg seed = (multiplier * seed + increment) `mod` modulus
  where
    multiplier = 1664525
    increment = 1013904223
    modulus = fromIntegral $ (2 :: Integer) ^ (32 :: Integer)

getIndexRounded :: Int -> [a] -> a
getIndexRounded i ls = ls !! (i `mod` length ls)

generateGrid :: Int -> (Int, Int) -> KeySequence -> Either String KeyGrid
generateGrid seed (rows, columns) hintKeys
  | rows * columns >= length hintKeys * length hintKeys = Left "Row/Column counts too high"
  | otherwise = Right $ (\row -> getKeySeq row <$> [0 .. columns - 1]) <$> [0 .. rows - 1]
  where
    allKeySeq = take numPairs . uniq $ generatePairs
    numPairs = rows * columns
    getKeySeq row col = allKeySeq !! (row * columns + col)
    randomNumbers = iterate lcg seed
    generatePairs =
      [ [getIndexRounded i hintKeys, getIndexRounded j hintKeys]
        | (i, j) <- zip randomNumbers (drop numPairs randomNumbers)
      ]

toKeyChar :: SDL.Keycode -> Maybe Char
toKeyChar = (`Map.lookup` keycodeCharMapping)

keycodeToInt :: SDL.Keycode -> Maybe Int
keycodeToInt = (`elemIndex` digitKeycodes)

isKeycodeDigit :: SDL.Keycode -> Bool
isKeycodeDigit = isJust . keycodeToInt

keycodeCharMapping :: Map.Map SDL.Keycode Char
keycodeCharMapping =
  Map.fromList
    [ (SDL.KeycodeA, 'A'),
      (SDL.KeycodeB, 'B'),
      (SDL.KeycodeC, 'C'),
      (SDL.KeycodeD, 'D'),
      (SDL.KeycodeE, 'E'),
      (SDL.KeycodeF, 'F'),
      (SDL.KeycodeG, 'G'),
      (SDL.KeycodeH, 'H'),
      (SDL.KeycodeI, 'I'),
      (SDL.KeycodeJ, 'J'),
      (SDL.KeycodeK, 'K'),
      (SDL.KeycodeL, 'L'),
      (SDL.KeycodeM, 'M'),
      (SDL.KeycodeN, 'N'),
      (SDL.KeycodeO, 'O'),
      (SDL.KeycodeP, 'P'),
      (SDL.KeycodeQ, 'Q'),
      (SDL.KeycodeR, 'R'),
      (SDL.KeycodeS, 'S'),
      (SDL.KeycodeT, 'T'),
      (SDL.KeycodeU, 'U'),
      (SDL.KeycodeV, 'V'),
      (SDL.KeycodeW, 'W'),
      (SDL.KeycodeX, 'X'),
      (SDL.KeycodeY, 'Y'),
      (SDL.KeycodeZ, 'Z')
    ]

digitKeycodes :: [SDL.Keycode]
digitKeycodes =
  [ SDL.Keycode0,
    SDL.Keycode1,
    SDL.Keycode2,
    SDL.Keycode3,
    SDL.Keycode4,
    SDL.Keycode5,
    SDL.Keycode6,
    SDL.Keycode7,
    SDL.Keycode8,
    SDL.Keycode9
  ]
