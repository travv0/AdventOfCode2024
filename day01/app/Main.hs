module Main where

import Data.List (sort)

main :: IO ()
main = do
  input <- readFile "input.txt"
  let (list1, list2) = parseInput input
  putStrLn $ "Part 1: " ++ show (sumDifferences list1 list2)
  putStrLn $ "Part 2: " ++ show (sumSimilarityScores list1 list2)

parseInput :: String -> ([Int], [Int])
parseInput input = (map head rows, map last rows)
  where rows = map (map read . words) $ lines input

sumDifferences :: (Num a, Ord a) => [a] -> [a] -> a
sumDifferences list1 list2 = sum $
  zipWith (\l r -> abs (l - r)) (sort list1) (sort list2)

similarityScore :: (Num a, Ord a) => a -> [a] -> a
similarityScore num nums = num * count
  where count = fromIntegral $ length $ filter (== num) nums

sumSimilarityScores :: (Num a, Ord a) => [a] -> [a] -> a
sumSimilarityScores list1 list2 = sum $ map (`similarityScore` list2) list1
