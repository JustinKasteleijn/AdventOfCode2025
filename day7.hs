import Control.Applicative (Alternative (..))
import Data.Array
import Data.List (foldl', nub)
import Data.Map qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Set qualified as S
import Debug.Trace (trace)
import Parser
import Utilities

type Position = (Int, Int)

data Grid = Grid
  { obstacles :: S.Set Position,
    origin :: Position,
    width :: Int,
    height :: Int
  }
  deriving (Show)

parseGrid :: Parser Grid
parseGrid = do
  rows <- sepBy1 (many (satisfy (/= '\n'))) newline
  let rows' = filter (not . null) rows

  let h = length rows'
  let w = maximum (map length rows')

  let coords =
        [ ((r, c), ch)
          | (r, row) <- zip [0 ..] rows',
            (c, ch) <- zip [0 ..] row
        ]

  let origin = head [(r, c) | ((r, c), 'S') <- coords]

  return
    Grid
      { obstacles = S.fromList [(r, c) | ((r, c), '^') <- coords],
        origin = origin,
        width = w,
        height = h
      }

solve :: Grid -> Int
solve grid = go (S.empty, 0, [origin grid])
  where
    obs = obstacles grid
    h = height grid
    w = width grid

    go :: (S.Set Position, Int, [Position]) -> Int
    go (visited, count, []) = count
    go (visited, count, p@(y, x) : rest)
      | not (inBounds p h w) = go (visited, count, rest)
      | p `S.member` visited = go (visited, count, rest)
      | p `S.member` obs =
          let visited' = S.insert p visited
              newStack = (y + 1, x - 1) : (y + 1, x + 1) : rest
              count' = count + 1
           in go (visited', count', newStack)
      | otherwise =
          let visited' = S.insert p visited
              newStack = (y + 1, x) : rest
           in go (visited', count, newStack)

inBounds :: Position -> Int -> Int -> Bool
inBounds (y, x) height width =
  y >= 0
    && y < height
    && x >= 0
    && x < width

solve2 :: Grid -> Int
solve2 grid = downFrom (y0 + 1, x0)
  where
    obs = obstacles grid
    (y0, x0) = origin grid
    maxY = height grid - 1
    maxX = width grid - 1

    memo :: M.Map Position Int
    memo = M.fromList [(pos, count pos) | pos <- S.toList obs]

    count :: Position -> Int
    count pos@(y, x) =
      let left = if x > 0 then downFrom (y + 1, x - 1) else 1
          right = if x < maxX then downFrom (y + 1, x + 1) else 1
       in left + right

    downFrom :: Position -> Int
    downFrom pos@(y, x)
      | y > maxY || x < 0 || x > maxX = 1
      | pos `S.member` obs = memo M.! pos
      | otherwise = downFrom (y + 1, x)

main :: IO ()
main = do
  let testInput = unwrapParser parseGrid input
  print $ solve testInput
  print $ solve2 testInput

  input' <- readFile "day7.txt"
  let input'' = unwrapParser parseGrid input'

  print $ solve input''
  print $ solve2 input''

input :: String
input =
  ".......S.......\n\
  \...............\n\
  \.......^.......\n\
  \...............\n\
  \......^.^......\n\
  \...............\n\
  \.....^.^.^.....\n\
  \...............\n\
  \....^.^...^....\n\
  \...............\n\
  \...^.^...^.^...\n\
  \...............\n\
  \..^...^.....^..\n\
  \...............\n\
  \.^.^.^.^.^...^.\n\
  \...............\n"
