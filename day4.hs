import Benchmark
import Data.List (foldl')
import Data.Set qualified as S
import Parser
import Utilities

-- Data

type Position = (Int, Int)

type Graph = S.Set Position

-- Parsing

parseInput :: String -> Graph
parseInput input =
  S.fromList
    [ (x, y)
      | (y, line) <- zip [0 ..] (lines input),
        (x, c) <- zip [0 ..] line,
        c == '@'
    ]

mooreNeighborhood :: Position -> [Position]
mooreNeighborhood (x, y) =
  [ (x + dx, y + dy)
    | dx <- [-1, 0, 1],
      dy <- [-1, 0, 1],
      (dx, dy) /= (0, 0)
  ]

data FoldState = FS
  { paperRolls :: !Int,
    graphAcc :: !Graph
  }

solve :: (Position -> Graph -> Graph) -> Graph -> Int -> (Int, Graph)
solve update graph n =
  let final = S.foldl' step (FS 0 graph) graph
   in (paperRolls final, graphAcc final)
  where
    step :: FoldState -> Position -> FoldState
    step (FS acc' graph') pos'
      | countNeighbors (mooreNeighborhood pos') 0 < n =
          let new = update pos' graph'
           in FS (acc' + 1) new
      | otherwise = FS acc' graph'

    countNeighbors :: [Position] -> Int -> Int
    countNeighbors [] acc = acc
    countNeighbors (p : ps) acc
      | acc >= n = acc
      | S.member p graph = countNeighbors ps (acc + 1)
      | otherwise = countNeighbors ps acc

solve1 :: Graph -> Int -> Int
solve1 graph n = fst $ solve (\_ g -> g) graph n

solve2 :: Graph -> Int -> Int
solve2 graph n =
  let (res, graph') = solve S.delete graph n
   in if res > 0
        then res + solve2 graph' n
        else res

main :: IO ()
main = do
  -- print $ parseInput testInput
  print $ solve1 (parseInput testInput) 4
  print $ solve2 (parseInput testInput) 4

  input <- readFile "day4.txt"
  parsed <- timeIt "Parsing: " (parseInput input)
  result1 <- timeIt "Day 4 Part 1" (solve1 parsed 4)
  result2 <- timeIt "Day 4 Part 2" (solve2 parsed 4)
  print result1
  print result2

-- Testing

testInput :: String
testInput =
  "..@@.@@@@.\n\
  \@@@.@.@.@@\n\
  \@@@@@.@.@@\n\
  \@.@@@@..@.\n\
  \@@.@@@@.@@\n\
  \.@@@@@@@.@\n\
  \.@.@.@.@@@\n\
  \@.@@@.@@@@\n\
  \.@@@@@@@@.\n\
  \@.@.@@@.@.\n"
