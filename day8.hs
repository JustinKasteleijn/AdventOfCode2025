import Data.IntMap.Strict qualified as IM
import Data.List (group, sort, sortBy, sortOn)
import Data.Ord (Down (..), comparing)
import Parser

data V3 a = V3 !a !a !a
  deriving (Show, Eq)

data Edge = Edge
  { dist :: !Int,
    index1 :: !Int,
    index2 :: !Int
  }

parserV3 :: Parser (V3 Int)
parserV3 = do
  [x, y, z] <- sepBy1 int (char ',')
  return $ V3 x y z

parse' :: Parser [V3 Int]
parse' = lines1 parserV3

euclidInt :: V3 Int -> V3 Int -> Int
euclidInt (V3 x y z) (V3 x' y' z') =
  round . sqrt . fromIntegral $
    (x - x') ^ 2 + (y - y') ^ 2 + (z - z') ^ 2

generateEdges :: [V3 Int] -> [Edge]
generateEdges vs =
  [ Edge {dist = euclidInt (vs !! i) (vs !! j), index1 = i, index2 = j}
    | i <- [0 .. n - 1],
      j <- [i + 1 .. n - 1]
  ]
  where
    n = length vs

find :: IM.IntMap Int -> Int -> Int
find parent x =
  let p = parent IM.! x
   in if p == x then x else find parent p

union :: IM.IntMap Int -> Int -> Int -> IM.IntMap Int
union parent x y =
  let px = find parent x
      py = find parent y
   in if px == py
        then parent
        else IM.insert px py parent

initParent :: Int -> IM.IntMap Int
initParent n = IM.fromList [(i, i) | i <- [0 .. n - 1]]

kruskal :: Int -> [Edge] -> [Edge]
kruskal n edges = go (initParent n) (sortOn dist edges) []
  where
    go _ [] mst = mst
    go parent (e : es) mst
      | find parent (index1 e) /= find parent (index2 e) =
          go (union parent (index1 e) (index2 e)) es (e : mst)
      | otherwise = go parent es mst

solve1 :: [V3 Int] -> Int -> Int
solve1 vs numEdges = product . take 3 . sortOn Down $ componentSizes finalParent
  where
    n = length vs
    edgesSorted = take numEdges $ sortOn dist (generateEdges vs)
    finalParent = foldl (\parent e -> union parent (index1 e) (index2 e)) (initParent n) edgesSorted

componentSizes :: IM.IntMap Int -> [Int]
componentSizes parent =
  let reps = map (find parent) (IM.keys parent)
   in map length . group . sort $ reps

numCircuits :: IM.IntMap Int -> Int
numCircuits parent =
  length
    . group
    . sort
    $ map (find parent) (IM.keys parent)

lastEdge :: [V3 Int] -> Edge
lastEdge vs = go (initParent n) (sortOn dist $ generateEdges vs)
  where
    n = length vs
    go parent (e : es)
      | find parent (index1 e) /= find parent (index2 e) =
          let newParent = union parent (index1 e) (index2 e)
           in if numCircuits newParent == 1
                then e
                else go newParent es
      | otherwise = go parent es

solve2 :: [V3 Int] -> Int
solve2 vs =
  let e = lastEdge vs
      V3 x1 _ _ = vs !! index1 e
      V3 x2 _ _ = vs !! index2 e
   in x1 * x2

main :: IO ()
main = do
  let parsed = unwrapParser parse' input
  print $ solve1 parsed 10
  print $ solve2 parsed

  input' <- readFile "day8.txt"
  let parsed' = unwrapParser parse' input'
  print $ solve1 parsed' 1000
  print $ solve2 parsed'

input :: String
input =
  unlines
    [ "162,817,812",
      "57,618,57",
      "906,360,560",
      "592,479,940",
      "352,342,300",
      "466,668,158",
      "542,29,236",
      "431,825,988",
      "739,650,466",
      "52,470,668",
      "216,146,977",
      "819,987,18",
      "117,168,530",
      "805,96,715",
      "346,949,466",
      "970,615,88",
      "941,993,340",
      "862,61,35",
      "984,92,344",
      "425,690,689"
    ]
