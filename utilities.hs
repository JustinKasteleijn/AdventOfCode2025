module Utilities where

testPart :: forall a. (Show a, Eq a) => Int -> String -> (String -> a) -> a -> String
testPart part input f expected =
  let actual = f input
   in if actual == expected
        then "Part " ++ show part ++ " succeeded"
        else "Expected: '" ++ show expected ++ "' got '" ++ show actual ++ "'"
