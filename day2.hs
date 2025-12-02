import Parser
import Utilities

-- Structure

data Range = Range
  { from :: Int,
    to :: Int
  }
  deriving (Show)

tupleToRange :: (Int, Int) -> Range
tupleToRange (x, y) = Range {from = x, to = y}

parseRange :: Parser Range
parseRange =
  tupleToRange
    <$> splitOn '-' int

parseRanges :: Parser [Range]
parseRanges = sepBy1 parseRange (char ',')

verifyRanges :: [Range] -> Int
verifyRanges xs = sum $ map verifyRange xs

verifyRange :: Range -> Int
verifyRange r = sum $ filter symmetry [from r .. to r]
  where
    symmetry :: (Show a) => a -> Bool
    symmetry n =
      let s = show n
          len = length s
       in even len && take (len `div` 2) s == drop (len `div` 2) s

verifyRange2 :: Range -> Int
verifyRange2 r = sum $ filter isSymmetric [from r .. to r]
  where
    isSymmetric :: Int -> Bool
    isSymmetric n = any hasRepeatingBlock [1 .. numDigits `div` 2]
      where
        s = show n
        numDigits = length s

        hasRepeatingBlock :: Int -> Bool
        hasRepeatingBlock k
          | numDigits `mod` k /= 0 = False
          | otherwise =
              let chunks' = chunks k s
               in all (== head chunks') chunks'

        chunks :: Int -> String -> [String]
        chunks k "" = []
        chunks k str = take k str : chunks k (drop k str)

verifyRanges2 :: [Range] -> Int
verifyRanges2 xs = sum $ map verifyRange2 xs

solve1 :: String -> Int
solve1 s =
  let r = unwrapParser parseRanges s
   in verifyRanges r

solve2 :: String -> Int
solve2 s =
  let r = unwrapParser parseRanges s
   in verifyRanges2 r

main :: IO ()
main = do
  -- print $ unwrapParser parseRanges ranges
  print $ testPart1 1227775554
  print $ testPart2 4174379265

  input <- readFile "day2.txt"
  print "Results commented out becausde they take a second to run which is annoying when refactoring the code :)"

-- print $ solve1 input
-- print $ solve2 input

-- Testing

testPart1 :: Int -> String
testPart1 = testPart 1 ranges solve1

testPart2 :: Int -> String
testPart2 = testPart 2 ranges solve2

ranges :: String
ranges = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"
