module Day2.Main where

import           Data.List  (find)
import           Data.Maybe (mapMaybe)

main :: IO ()
main = do
  nums <- fmap (fmap read . words) . lines <$> getContents
  print $ solve1 nums
  print $ solve2 nums

-- |
-- >>> solve1 [[5,1,9,5],[7,5,3],[2,4,6,8]]
-- 18
solve1 :: [[Int]] -> Int
solve1 = sum . fmap ((-) <$> maximum <*> minimum)

-- |
-- >>> solve2 [[5,9,2,8],[9,4,7,3],[3,8,6,5]]
-- 9
solve2 :: [[Int]] -> Int
solve2 = sum . mapMaybe (fmap (uncurry div) . find ((&&) <$> (==) 0 . uncurry mod <*> uncurry (/=)) . ((<*>) . fmap (,) <*> id))
