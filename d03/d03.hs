import Data.List (foldl', group, maximumBy, sort, transpose)
import Data.Ord (comparing)

main = do
  parsed <- map parse . lines <$> readFile "input"
  print $ part1 parsed
  print $ part2 parsed

-- expression style with let
part1 parsed =
  let reduced = map mode . transpose $ parsed
      gamma = toInt reduced
      epsilon = toInt . map not $ reduced
   in gamma * epsilon

parse :: [Char] -> [Bool]
parse =
  map parseBit
  where
    parseBit '0' = False
    parseBit '1' = True
    parseBit _ = error "invalid bit"

-- >>> mode [True, False]
-- True
-- >>> mode [False, True, False]
-- False
mode :: [Bool] -> Bool
-- group splits by contiguous runs
-- comparing conveniently gives an Ordering, like Python's sort key parameter
-- maximumBy uses the ordering from comparing
mode = head . maximumBy (comparing tuple) . group . sort
  where
    -- most common, but tiebreak by picking True
    tuple xs = (length xs, head xs)

-- >>> toInt [True]
-- 1
-- >>> toInt [True, False]
-- 2
toInt :: [Bool] -> Int
-- foldl' is a strict (non-lazy) foldl for efficient tail call optimization
toInt = foldl' (\x y -> 2 * x + bit y) 0
  where
    bit True = 1
    bit False = 0

-- >>> winnow mode [[False], [True]]
-- [True]
-- >>> winnow mode . map parse $ ["10", "11", "01", "01"]
-- [True,True]
-- ambiguous results are not handled
-- >>> winnow mode [[True], [True], [False]]
-- Prelude.head: empty list
-- >>> winnow mode . map parse $ ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- [True,False,True,True,True]
-- >>> winnow (not . mode) . map parse $ ["00100","11110","10110","10111","10101","01111","00111","11100","10000","11001","00010","01010"]
-- [False,True,False,True,False]
winnow :: ([Bool] -> Bool) -> [[Bool]] -> [Bool]
winnow _ [] = error "No final value"
winnow _ [xs] = xs
-- construct from first bit and further winnowed bits
winnow criteria xs = first : winnow criteria (map tail filtered)
  where
    -- most common first bit
    first = criteria . map head $ xs
    -- filter to values with that first bit
    filtered = filter (\ys -> head ys == first) xs

-- declaration style with where
part2 parsed =
  oxygen * co2
  where
    oxygen = toInt . winnow mode $ parsed
    -- not . mode will select the least common bit
    co2 = toInt . winnow (not . mode) $ parsed
