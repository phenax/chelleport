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

generateKeyCells :: (Int, Int) -> KeySequence -> KeyGrid
generateKeyCells (rows, columns) hintKeys =
  (\row -> getCellSeq row <$> [1 .. columns]) <$> [1 .. rows]
  where
    getCellSeq row col = [getPrefixHoriz row col, getPrefixVert row col, getKey row col]
    getKey row col = hintKeys !! index
      where
        index = (secRow * (columns `div` 2) + secCol) `mod` length hintKeys
        secCol = (col - 1) `mod` (columns `div` 2)
        secRow = (row - 1) `mod` (rows `div` 2)
    getPrefixHoriz _row col
      | col <= (columns `div` 2) = 'H'
      | otherwise = 'L'
    getPrefixVert row _col
      | row <= (rows `div` 2) = 'K'
      | otherwise = 'J'

toKeyChar :: SDL.Keycode -> Maybe Char
toKeyChar = (`Map.lookup` keycodeMapping)

eventToKeycode :: SDL.KeyboardEventData -> SDL.Keycode
eventToKeycode = SDL.keysymKeycode . SDL.keyboardEventKeysym

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
