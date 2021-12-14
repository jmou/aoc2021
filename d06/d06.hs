-- particularly elegant: https://www.reddit.com/r/adventofcode/comments/r9z49j/comment/hnkb8il/

-- allows type ascription in patterns like left of <-
{-# LANGUAGE ScopedTypeVariables #-}

import Data.List (group, sort)
import Data.List.Split (splitOn)

sample :: [Int]
sample = [3, 4, 3, 1, 2]

-- >>> tick sample
-- [2,3,2,0,1]
-- >>> tick . tick $ sample
-- [1,2,1,6,8,0]
tick :: [Int] -> [Int]
-- or: tick = foldr (\x -> (++) (if x == 0 then [6, 8] else [x - 1])) []
tick [] = []
tick (x : xs) = (if x == 0 then [6, 8] else [x - 1]) ++ tick xs

newtype State = State [Int] deriving (Show)

-- >>> fromList sample
-- State [0,1,1,2,1,0,0,0,0]
fromList :: [Int] -> State
-- convert a list of lifetimes into a positional list of counts
fromList = State . construct 0 . bag
  where
    -- group values, then count them (ad hoc bag/multiset)
    bag = sort . map (\(y : ys) -> (y, length ys + 1)) . group . sort :: [Int] -> [(Int, Int)]
    -- zero-fill any missing counts
    place pos ((timer, count) : xs) | pos == timer = (count, xs)
    place _ xs = (0, xs)
    -- fill exactly to length 9
    construct 9 [] = []
    construct 9 _ = error "invalid state"
    construct pos xs =
      -- place each count
      let (count, rest) = place pos xs
       in count : construct (pos + 1) rest

-- >>> tickFast . tickFast $ fromList sample
-- State [1,2,1,0,0,0,1,0,1]
tickFast :: State -> State
-- destructure the entire list; vector may work better here
tickFast (State [x0, x1, x2, x3, x4, x5, x6, x7, x8]) =
  State [x1, x2, x3, x4, x5, x6, x7 + x0, x8, x0]
tickFast _ = error "state wrong size"

-- inconvenient to access into newtyped value
count :: State -> Int
count (State xs) = sum xs

-- >>> length $ simulate tick 18 sample
-- 26
-- >>> length $ simulate tick 80 sample
-- 5934
-- >>> count $ simulate tickFast 256 (fromList sample)
-- 26984457539
simulate :: (a -> a) -> Int -> a -> a
simulate fn 0 state = state
simulate fn n state = simulate fn (n - 1) (fn state)

main = do
  initial :: [Int] <- map read . splitOn "," <$> readFile "input"
  print $ length $ simulate tick 80 initial
  print $ count $ simulate tickFast 256 (fromList initial)
