import Control.Applicative (Alternative (..))
import Parser
import Utilities (testPart)

-- Types and instances

data Rotation
  = L Int
  | R Int
  deriving (Show)

newtype Dial = Dial Int
  deriving (Eq, Show)

mkDial :: Int -> Dial
mkDial n = Dial (n `mod` 100)

unwrap :: Dial -> Int
unwrap (Dial n) = n

instance Num Dial where
  (Dial a) + (Dial b) = mkDial (a + b)
  (Dial a) - (Dial b) = mkDial (a - b)
  (Dial a) * (Dial b) = mkDial (a * b)
  abs (Dial a) = Dial (abs a)
  signum (Dial a) = Dial (signum a)
  fromInteger n = mkDial (fromInteger n)

initialDial :: Dial
initialDial = mkDial 50

rotate :: Rotation -> Dial -> Dial
rotate (L n) dial = dial - fromIntegral n
rotate (R n) dial = dial + fromIntegral n

delta :: Rotation -> Int
delta (R n) = n
delta (L n) = -n

-- Parsers

parseRotation :: Parser Rotation
parseRotation =
  choice
    [ L <$> (char 'L' *> int),
      R <$> (char 'R' *> int)
    ]

parseRotations :: Parser [Rotation]
parseRotations = lines1 parseRotation

-- Solvers

data FoldState = FS
  { acc :: !Int,
    dial :: !Dial
  }

solve1 :: String -> Int
solve1 =
  acc
    . foldl countZero (FS 0 initialDial)
    . unwrapParser parseRotations
  where
    countZero :: FoldState -> Rotation -> FoldState
    countZero (FS acc dial) r =
      let dial' = rotate r dial
       in if unwrap dial' == 0
            then FS (acc + 1) dial'
            else FS acc dial'

solve2 :: String -> Int
solve2 =
  acc
    . foldl countFlowing (FS 0 initialDial)
    . unwrapParser parseRotations
  where
    countFlowing :: FoldState -> Rotation -> FoldState
    countFlowing (FS acc dial) r =
      let d = delta r
          step = if d > 0 then 1 else -1
          positions = take (abs d) $ tail $ iterate (\x -> x + fromIntegral step) dial
          hitsDuring = length $ filter ((== 0) . unwrap) positions
          dial' = dial + fromIntegral d
       in FS (acc + hitsDuring) dial'

--  Entry point
main :: IO ()
main = do
  print $ testPart1 3
  print $ testPart2 6

  input <- readFile "day1.txt"
  print $ solve1 input
  print $ solve2 input

-- Testing

testPart1 :: Int -> String
testPart1 = testPart 1 testInput solve1

testPart2 :: Int -> String
testPart2 = testPart 2 testInput solve2

testInput :: String
testInput =
  unlines
    [ "L68",
      "L30",
      "R48",
      "L5",
      "R60",
      "L55",
      "L1",
      "L99",
      "R14",
      "L82"
    ]
