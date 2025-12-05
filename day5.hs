import Data.List (foldl', sortOn)
import Parser
import Utilities

type Ingredient = Int

type Range = (Int, Int)

parseRange :: Parser Range
parseRange = splitOn '-' int

parseRanges :: Parser [Range]
parseRanges = lines1 parseRange

parseIngredient :: Parser Ingredient
parseIngredient = int

parseIngredients :: Parser [Ingredient]
parseIngredients = lines1 parseIngredient

parse' :: Parser ([Range], [Ingredient])
parse' = splitOn' "\n\n" parseRanges parseIngredients

solve1 :: [Range] -> [Ingredient] -> Int
solve1 ranges ingredients =
  length $ filter (`inAnyRange` ranges) ingredients
  where
    inAnyRange i = any (\(l, r) -> i >= l && i <= r)

solve2 :: [Range] -> Int
solve2 =
  sum
    . map count
    . foldl' mergeOverlapping []
    . sortOn fst
  where
    mergeOverlapping :: [Range] -> Range -> [Range]
    mergeOverlapping [] r = [r]
    mergeOverlapping acc@((l', r') : rest) (l, r)
      | r < l' = (l, r) : acc
      | l > r' = (l, r) : acc
      | otherwise = (min l l', max r r') : rest

    count :: Range -> Int
    count (l, r) = r - l + 1

main :: IO ()
main = do
  input <- readFile "day5.txt"
  let (ranges, ingredients) = unwrapParser parse' input
  let (ranges', ingredients') = unwrapParser parse' testInput
  print $ solve1 ranges ingredients
  print $ solve2 ranges

testInput :: String
testInput =
  "3-5\n\
  \10-14\n\
  \16-20\n\
  \12-18\n\
  \\n\
  \1\n\
  \5\n\
  \8\n\
  \11\n\
  \17\n\
  \32\n"
