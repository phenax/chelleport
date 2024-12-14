module Chelleport.KeySequence where

import Data.List (isPrefixOf, nub)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import qualified SDL

-- padded :: Int -> a -> [a] -> [a]
-- padded 0 _ ls = ls
-- padded n x ls
--   | length ls > n = ls
--   | otherwise = padded (n - 1) x (ls ++ [x])

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ (x : _) = x

nextChars :: [Char] -> [[[Char]]] -> Maybe [Char]
nextChars keys cells =
  case matches of
    [] -> Nothing
    _ -> Just $ nub result
  where
    matches = concatMap (filter (isPrefixOf keys)) cells
    result = concatMap (take 1 . drop (length keys)) matches

findMatchPosition :: [Char] -> [[[Char]]] -> Maybe (Int, Int)
findMatchPosition keys = findWithIndex findMatch 0
  where
    findMatch row =
      fst <$> findWithIndex (\c -> if c == keys then Just () else Nothing) 0 row

    findWithIndex :: (x -> Maybe r) -> Int -> [x] -> Maybe (Int, r)
    findWithIndex _pred _index [] = Nothing
    findWithIndex predicate index (x : ls) =
      case predicate x of
        Just item -> Just (index, item)
        Nothing -> findWithIndex predicate (index + 1) ls

isValidKey :: SDL.Keycode -> Bool
isValidKey key = Map.member key keycodeMapping

generateKeyCells :: (Int, Int) -> [Char] -> [[[Char]]]
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
toKeyChar key = Map.lookup key keycodeMapping

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
