import Data.Bifunctor (Bifunctor (bimap))
import Data.Functor (($>))
import Data.List (foldl', transpose)
import Parser
import Utilities

data Operator
  = Add
  | Mul
  deriving (Show)

toOperator :: Operator -> (Int -> Int -> Int)
toOperator Add = (+)
toOperator Mul = (*)

parseNumbers :: Parser [Int]
parseNumbers =
  spaces0
    *> sepBy1 int spaces1
    <* spaces0

parseOperators :: Parser [Operator]
parseOperators =
  sepBy1
    ( choice
        [ char '+' $> Add,
          char '*' $> Mul
        ]
    )
    spaces1

parse' :: Parser ([[Int]], [Operator])
parse' = do
  nums <- sepBy1 parseNumbers newline
  newline
  ops <- parseOperators
  return (nums, ops)

solve :: [[Int]] -> [Operator] -> Int
solve xss ops =
  sum $
    zipWith combine ops xss
  where
    combine :: Operator -> [Int] -> Int
    combine Add = sum
    combine Mul = product

solve1 :: [[Int]] -> [Operator] -> Int
solve1 xss = solve (transpose xss)

solve2 :: [[Int]] -> [Operator] -> Int
solve2 nums ops = solve nums (reverse ops)

parseInputPartTwo :: String -> ([String], [[String]])
parseInputPartTwo =
  bimap (split ' ') (split "" . map (filter (/= ' ')) . transpose)
    . (\x -> (last x, init x))
    . lines

toInt :: String -> Int
toInt = read

main :: IO ()
main = do
  let (numbers, operators) = unwrapParser parse' testInput
  print $ solve1 numbers operators

  let (_, numbers2) = parseInputPartTwo testInput
  print (map (reverse . map toInt) numbers2)
  print $ solve2 (map (reverse . map toInt) numbers2) operators

  input <- readFile "day6.txt"
  let (numbers', operators') = unwrapParser parse' input
  let (_, numbers'') = parseInputPartTwo input
  print $ solve1 numbers' operators'
  print $ solve2 (map (reverse . map toInt) numbers'') operators'

  return ()

testInput :: String
testInput =
  "123 328  51 64 \n\
  \ 45 64  387 23 \n\
  \  6 98  215 314\n\
  \*   +   *   +  \n"
