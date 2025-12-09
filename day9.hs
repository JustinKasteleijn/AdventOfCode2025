import Benchmark (timeIt)
import Data.List (tails)
import Parser

data V2 a = V2 !a !a
  deriving (Show)

parseCoord :: Parser (V2 Int)
parseCoord =
  fmap
    (uncurry V2)
    (splitOn ',' int)

parseCoords :: Parser [V2 Int]
parseCoords = lines1 parseCoord

area :: (Num a) => V2 a -> V2 a -> a
area (V2 x y) (V2 x' y') =
  (abs (x - x') + 1) * (abs (y - y') + 1)

solve1 :: [V2 Int] -> Int
solve1 coords =
  maximum
    [ area v2 v2'
      | (v2 : rest) <- tails coords,
        v2' <- rest
    ]

-- Credits to NerdyPepper for helping me come up with this solution!
-- https://aoc.oppi.li/2.5-day-9.html#day-9
-- https://tangled.org/oppi.li/aoc/blob/main/src/2025/09.lhs
intersects :: (Num a, Ord a) => V2 a -> V2 a -> [V2 a] -> Bool
intersects (V2 x y) (V2 x' y') = not . all away . pairs
  where
    pairs (v2 : v2s) = zip (v2 : v2s) (v2s ++ [v2])
    away (V2 x'' y'', V2 x''' y''') =
      (max x'' x''' <= min x x')
        || (min x'' x''' >= max x x')
        || (max y'' y''' <= min y y')
        || (min y'' y''' >= max y y')

solve2 :: [V2 Int] -> Int
solve2 coords =
  maximum $
    [ area ci cj
      | (i, ci) <- indexed,
        (j, cj) <- indexed,
        j > i,
        not (intersects ci cj coords)
    ]
  where
    indexed :: [(Int, V2 Int)]
    indexed = zip [0 ..] coords

main :: IO ()
main = do
  let parsed = unwrapParser parseCoords input
  print parsed

  print $ solve1 parsed
  print $ solve2 parsed

  input' <- readFile "day9.txt"
  let parsed' = unwrapParser parseCoords input'
  result1 <- timeIt "Day 9, Part 1:" $ solve1 parsed'
  print result1

  result2 <- timeIt "Day 9, Part 2:" $ solve2 parsed'
  print result2

input :: String
input =
  "7,1\n\
  \11,1\n\
  \11,7\n\
  \9,7\n\
  \9,5\n\
  \2,5\n\
  \2,3\n\
  \7,3"
