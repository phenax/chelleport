module Chelleport.Utils where

import Data.List (nub)
import Foreign.C (CInt)

intToCInt :: Int -> CInt
intToCInt = fromIntegral

cIntToInt :: CInt -> Int
cIntToInt = fromIntegral

findWithIndex :: (x -> Maybe r) -> Int -> [x] -> Maybe (Int, r)
findWithIndex _predicate _index [] = Nothing
findWithIndex predicate index (x : ls) =
  case predicate x of
    Just item -> Just (index, item)
    Nothing -> findWithIndex predicate (index + 1) ls

uniq :: (Eq a) => [a] -> [a]
uniq = nub

isEmpty :: [a] -> Bool
isEmpty = null

isNotEmpty :: [a] -> Bool
isNotEmpty = not . isEmpty
