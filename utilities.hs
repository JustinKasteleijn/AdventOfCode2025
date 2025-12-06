module Utilities where

import Data.List (tails)

testPart :: forall a. (Show a, Eq a) => Int -> String -> (String -> a) -> a -> String
testPart part input f expected =
  let actual = f input
   in if actual == expected
        then "Part " ++ show part ++ " succeeded"
        else "Expected: '" ++ show expected ++ "' got '" ++ show actual ++ "'"

windows :: Int -> [a] -> [[a]]
windows n = filter (\x -> length x == n) . map (take n) . tails

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks n xs = take n xs : chunks n (drop n xs)

split :: (Eq a) => a -> [a] -> [[a]]
split c arr = splitOnHelper c arr [] []
  where
    splitOnHelper c [] acc ret = if null acc then ret else reverse acc : ret
    splitOnHelper c (x : xs) acc ret
      | c == x && null acc = splitOnHelper c xs [] ret
      | c == x = splitOnHelper c xs [] (reverse acc : ret)
      | otherwise = splitOnHelper c xs (x : acc) ret
