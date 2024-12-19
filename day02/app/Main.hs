module Main where

import Data.List (foldl', elemIndex)
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "input.txt"
  let reports = parseInput input
  putStrLn $ "Part 1: " ++ show (countSafe reports)
  putStrLn $ "Part 2: " ++ show (countSafeWithDampener reports)

parseInput :: String -> [[Int]]
parseInput = map (map read . words) . lines

isSafe :: (Ord a, Num a) => [a] -> Bool
isSafe nums =
  all (\(l, r) -> l `compare` r == firstComp && abs (r - l) > 0 && abs (r - l) <= 3) (zip nums (tail nums))
    where firstComp = head nums `compare` (nums !! 1)

countSafe :: (Ord a, Num a) => [[a]] -> Int
countSafe = length . filter isSafe

data Dampener = Unsafe | Dampened | Safe deriving (Eq, Ord, Enum, Show)

isSafeWithDampener :: (Ord a, Num a, Show a) => [a] -> Bool
isSafeWithDampener nums = isSafeWithDampener' False compDirection nums
  where compDirection = smallestNumPosition `compare` largestNumPosition
        smallestNumPosition = elemIndex (minimum nums) nums
        largestNumPosition = elemIndex (maximum nums) nums

isSafeWithDampener' :: (Ord a, Num a, Show a) => Bool -> Ordering -> [a] -> Bool
isSafeWithDampener' _ _ [] = True
isSafeWithDampener' _ _ [_] = True
isSafeWithDampener' numWasSkipped compDirection (l:r:rest)
  | compDirection == l `compare` r && abs (r - l) > 0 && abs (r - l) <= 3 =
    isSafeWithDampener' numWasSkipped compDirection (r:rest)
  | otherwise =
    not numWasSkipped
      -- && isSafeWithDampener' True compDirection (r:rest)
      && isSafeWithDampener' True compDirection (l:rest)

-- isSafeWithDampener :: (Ord a, Num a) => [a] -> Bool
-- isSafeWithDampener nums =
--   foldl' (\acc (l, r) ->
--             if acc /= Unsafe && l `compare` r == compDirection && abs (r - l) > 0 && abs (r - l) <= 3
--                then acc
--                else if acc > Unsafe then pred acc else acc)
--          Safe
--          (zip nums (tail nums))
--       /= Unsafe
    -- where compDirection = smallestNumPosition `compare` largestNumPosition
    --       smallestNumPosition = elemIndex (minimum nums) nums
    --       largestNumPosition = elemIndex (maximum nums) nums

countSafeWithDampener :: (Ord a, Num a, Show a) => [[a]] -> Int
countSafeWithDampener = length . filter isSafeWithDampener
