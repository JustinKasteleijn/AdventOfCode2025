import Data.List (foldl')
import Debug.Trace
import Parser
import Utilities

type Bank = [Int]

parseBank :: Parser Bank
parseBank = digitsAsList

parseBanks :: Parser [Bank]
parseBanks = lines1 parseBank

turnOnBatteries :: Int -> Bank -> Int
turnOnBatteries n bank = foldl' (\acc d -> acc * 10 + d) 0 (pickLargest n bank)
  where
    pickLargest :: Int -> Bank -> [Int]
    pickLargest 0 _ = []
    pickLargest n bank' =
      let takeUntil = length bank' - n + 1
          (maxVal, pos) = maxWithPos (take takeUntil bank')
          restBank = drop (pos + 1) bank'
       in maxVal : pickLargest (n - 1) restBank

    maxWithPos :: Bank -> (Int, Int)
    maxWithPos bank =
      foldl'
        ( \(max, pos) (curr, pos') ->
            if curr > max
              then (curr, pos')
              else (max, pos)
        )
        (0, 0)
        (zip bank [0 ..])

solve1 :: String -> Int
solve1 input =
  let p = unwrapParser parseBanks input
   in sum $ map (turnOnBatteries 2) p

solve2 :: String -> Int
solve2 input =
  let p = unwrapParser parseBanks input
   in sum $ map (turnOnBatteries 12) p

main :: IO ()
main = do
  print $ testPart1 357
  print $ testPart2 3121910778619

  input <- readFile "day3.txt"
  print $ solve1 input
  print $ solve2 input

testPart1 :: Int -> String
testPart1 = testPart 1 testInput solve1

testPart2 :: Int -> String
testPart2 = testPart 2 testInput solve2

testInput :: String
testInput =
  "987654321111111\n\
  \811111111111119\n\
  \234234234234278\n\
  \818181911112111"
