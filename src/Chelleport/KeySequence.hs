module Chelleport.KeySequence where

import Chelleport.Types (KeyGrid, KeySequence)
import Chelleport.Utils (findWithIndex, uniq)
import Control.Monad (guard)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import qualified SDL

nextChars :: KeySequence -> KeyGrid -> Maybe [Char]
nextChars keySequence cells =
  case matches of
    [] -> Nothing
    _ -> Just nextCharactersInSequence
  where
    matches = concatMap (filter $ isPrefixOf keySequence) cells
    nextCharactersInSequence = uniq $ concatMap (take 1 . drop (length keySequence)) matches

findMatchPosition :: KeySequence -> KeyGrid -> Maybe (Int, Int)
findMatchPosition keySequence = findWithIndex searchRows 0
  where
    searchRows = fmap fst . findWithIndex searchInRow 0
    searchInRow = guard . (== keySequence)

isValidKey :: SDL.Keycode -> Bool
isValidKey = (`Map.member` keycodeMapping)

-- Linear Congruential Generator
lcg :: Int -> Int
lcg seed = (a * seed + c) `mod` fromIntegral m
  where
    a = 1664525
    c = 1013904223
    m = (2 :: Integer) ^ (32 :: Integer)

getIndexRounded :: Int -> [a] -> a
getIndexRounded i ls = ls !! (i `mod` length ls)

generateGrid :: Int -> (Int, Int) -> KeySequence -> Maybe KeyGrid
generateGrid seed (rows, columns) hintKeys
  | rows * columns > length hintKeys * length hintKeys = Nothing
  | otherwise = Just $ (\row -> getKeySeq row <$> [0 .. columns - 1]) <$> [0 .. rows - 1]
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
toKeyChar = (`Map.lookup` keycodeMapping)

keycodeMapping :: Map.Map SDL.Keycode Char
keycodeMapping =
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
      (SDL.KeycodeZ, 'Z'),
      (SDL.Keycode0, '0'),
      (SDL.Keycode1, '1'),
      (SDL.Keycode2, '2'),
      (SDL.Keycode3, '3'),
      (SDL.Keycode4, '4'),
      (SDL.Keycode5, '5'),
      (SDL.Keycode6, '6'),
      (SDL.Keycode7, '7'),
      (SDL.Keycode8, '8'),
      (SDL.Keycode9, '9')
    ]
