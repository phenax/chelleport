module Chelleport.Utils where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.List (nub)
import Data.Time.Clock.System (SystemTime (systemNanoseconds), getSystemTime)
import qualified Debug.Trace as Debug
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

benchmark :: (MonadIO m) => String -> m a -> m a
benchmark msg m = do
  start <- systemNanoseconds <$> liftIO getSystemTime
  result <- m
  end <- systemNanoseconds <$> liftIO getSystemTime
  Debug.traceM $ msg ++ " (ms): " ++ show (fromIntegral (end - start) / 1_000_000.0 :: Double)
  pure result

itemAt :: [a] -> Int -> Maybe a
itemAt [] _ = Nothing
itemAt (x : _) 0 = Just x
itemAt (_ : xs) i = itemAt xs (i - 1)

clamp :: (Integral a) => (a, a) -> a -> a
clamp (low, high) n = max low (min high n)
