import Control.Applicative (Alternative (..))
import Data.Bits (shiftL, xor, (.|.))
import Data.Char (intToDigit)
import Data.Functor (($>))
import Data.List (foldl')
import Data.Sequence (Seq (..), (|>))
import Data.Sequence qualified as Seq
import Data.Set qualified as Set
import Numeric (showIntAtBase)
import Parser

type Mask = Int

toggle :: Mask -> Mask -> Mask
toggle = xor

initMask :: Mask
initMask = 0b0

indexesToMask :: [Int] -> Mask
indexesToMask = foldl' (\m i -> m .|. (1 `shiftL` i)) 0

bitsToMask :: [Int] -> Mask
bitsToMask bits =
  foldl'
    ( \m (i, b) ->
        if b == 1
          then m .|. (1 `shiftL` i)
          else m
    )
    0
    (zip [0 ..] bits)

data MachineInstructions = MI
  { goal :: !Mask,
    switches :: ![Mask],
    joltage :: ![Int]
  }
  deriving (Show)

parseGoal :: Parser Mask
parseGoal = do
  _ <- char '['
  bits <- many parseLight
  _ <- char ']'
  return $ bitsToMask bits
  where
    parseLight :: Parser Int
    parseLight =
      choice
        [ char '.' $> 0,
          char '#' $> 1
        ]

parseSwitches :: Parser [Mask]
parseSwitches = sepBy1 parsePos spaces0
  where
    parsePos :: Parser Mask
    parsePos = do
      _ <- char '('
      pos <- sepBy1 int (char ',')
      _ <- char ')'
      return $ indexesToMask pos

parseJoltage :: Parser [Int]
parseJoltage =
  char '{'
    *> sepBy1 int (char ',')
    <* char '}'

parseMachineInstruction :: Parser MachineInstructions
parseMachineInstruction =
  MI
    <$> (parseGoal <* spaces0)
    <*> (parseSwitches <* spaces0)
    <*> parseJoltage

parseMachineInstructions :: Parser [MachineInstructions]
parseMachineInstructions = lines1 parseMachineInstruction

toBinary :: Mask -> String
toBinary x =
  let s = showIntAtBase 2 intToDigit x ""
   in replicate (4 - length s) '0' ++ s

bfs :: Mask -> [Mask] -> Int
bfs goal switches = go (Seq.singleton (0, 0)) (Set.singleton 0)
  where
    go :: Seq (Mask, Int) -> Set.Set Mask -> Int
    go Seq.Empty _ = error "Goal not reachable"
    go ((state, steps) :<| rest) visited
      | state == goal = steps
      | otherwise =
          let nextStates = [state `xor` sw | sw <- switches, not (Set.member (state `xor` sw) visited)]
              visited' = foldr Set.insert visited nextStates
              rest' = rest <> Seq.fromList [(s, steps + 1) | s <- nextStates]
           in go rest' visited'

solve1 :: [MachineInstructions] -> Int
solve1 = sum . map (\mi -> bfs (goal mi) (switches mi))

main :: IO ()
main = do
  input <- readFile "day10.txt"
  let mi = unwrapParser parseMachineInstructions input
  print $ solve1 mi

singleLine :: String
singleLine = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}"

testInput :: String
testInput =
  unlines
    [ "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}",
      "[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}",
      "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"
    ]
