module Day3.Main where

main :: IO ()
main = do
  num <- read <$> getLine
  print $ solve1 num

-- |
-- >>> solve1 1
-- 0
-- >>> solve1 12
-- 3
-- >>> solve1 23
-- 2
-- >>> solve1 1024
-- 31
solve1 :: Int -> Int
solve1 1 = 0
solve1 n = x + (ys !! (n - 1 - (x * 2 - 1) ^ 2))
  where
    x = head $ dropWhile ((< n) . (^ 2) . (+ 1) . (* 2)) [0..]
    ys = concat . replicate 4 . tail $ (++) <$> reverse <*> tail $ [0..x]
