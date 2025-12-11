module Day11 (part1, part2) where

import Control.Arrow (first, second)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as HM
import Data.Maybe (fromMaybe)
import Utilities

data Status = InProgress | Visited {numPaths :: Int}

countPaths :: String -> String -> Map String [String] -> Int
countPaths start end connections = fst $ go HM.empty start
  where
    go :: Map String Status -> String -> (Int, Map String Status)
    go visited cur
      | cur == end = (1, visited)
      | otherwise = case HM.lookup cur visited of
          Just InProgress -> (0, visited)
          Just (Visited {numPaths}) -> (numPaths, visited)
          Nothing ->
            let (n, newVisited) =
                  foldl
                    ( \(count, vis) next ->
                        let (pathsFromNext, vis') = go vis next
                         in (count + pathsFromNext, vis')
                    )
                    (0, HM.insert cur InProgress visited)
                    (fromMaybe [] $ HM.lookup cur connections)
             in (n, HM.insert cur (Visited {numPaths = n}) newVisited)

parse :: String -> Map String [String]
parse = HM.fromList . map (second words . splitOnColon) . lines
  where
    splitOnColon :: String -> (String, String)
    splitOnColon s =
      let (a, rest) = break (== ':') s
       in case rest of
            ':' : ' ' : b -> (a, b)
            _ -> error "expected ': ' in line"

part1 :: String -> Int
part1 = countPaths "you" "out" . parse

part2 :: String -> Int
part2 s = (paths "svr" "dac" * paths "dac" "fft" * paths "fft" "out") + (paths "svr" "fft" * paths "fft" "dac" * paths "dac" "out")
  where
    connections = parse s
    paths start end = countPaths start end connections

main :: IO ()
main = do
  input <- readFile "Day11.txt"
  print $ part1 input
  print $ part2 input
