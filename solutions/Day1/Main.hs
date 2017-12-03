module Day1.Main where

import           Control.Monad (join)
import           Data.Function (on)
import           Data.List     (intersect)

main :: IO ()
main = do
  nums <- fmap (read . (: [])) <$> getLine
  print $ solve1 nums
  print $ solve2 nums

solve :: ([Int], [Int]) -> Int
solve = sum . fmap snd . uncurry (intersect `on` zipWith (,) [0..])

-- |
-- >>> solve1 [1,1,2,2]
-- 3
-- >>> solve1 [1,1,1,1]
-- 4
-- >>> solve1 [1,2,3,4]
-- 0
-- >>> solve1 [9,1,2,1,2,1,2,9]
-- 9
solve1 :: [Int] -> Int
solve1 = solve . ((,) <*> tail . join (++))

-- |
-- >>> solve2 [1,2,1,2]
-- 6
-- >>> solve2 [1,2,2,1]
-- 0
-- >>> solve2 [1,2,3,4,2,5]
-- 4
-- >>> solve2 [1,2,3,1,2,3]
-- 12
-- >>> solve2 [1,2,1,3,1,4,1,5]
-- 4
solve2 :: [Int] -> Int
solve2 = (* 2) . solve . (flip splitAt <*> (`div` 2) . length)
