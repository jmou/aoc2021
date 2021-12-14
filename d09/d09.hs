import Data.List (sortOn)

type Grid = [[Int]]

type Pos = (Int, Int)

sample = ["2199943210", "3987894921", "9856789892", "8767896789", "9899965678"]

sampleGrid :: Grid
sampleGrid = pad . map parseLine $ sample

parseLine :: String -> [Int]
-- (: []) effectively converts Char to String
parseLine = map (read . (: []))

-- pad with sentinels to avoid bounds checks
pad :: Grid -> Grid
pad rows@(top : _) = [topBot] ++ map padLeftRight rows ++ [topBot]
  where
    -- replicate generates a list of length
    topBot = replicate (length top + 2) 9
    padLeftRight row = [9] ++ row ++ [9]
pad [] = error "empty input"

adjacencies :: [Pos]
adjacencies = [(-1, 0), (0, -1), (1, 0), (0, 1)]

at :: Grid -> Pos -> Int
at grid (r, c) = grid !! r !! c

add :: Pos -> Pos -> Pos
add (a, b) (x, y) = (a + x, b + y)

-- >>> isLowPoint sampleGrid (1, 1)
-- False
-- >>> isLowPoint sampleGrid (1, 2)
-- True
isLowPoint :: Grid -> Pos -> Bool
isLowPoint grid pos = all (> at grid pos) neighbors
  where
    neighbors = [at grid (add pos d) | d <- adjacencies]

-- >>> basin sampleGrid (1, 2) []
-- [(2,1),(1,1),(1,2)]
-- >>> length $ basin sampleGrid (1, 10) []
-- 9
-- >>> length $ basin sampleGrid (3, 3) []
-- 14
-- >>> length $ basin sampleGrid (5, 7) []
-- 9
basin :: Grid -> Pos -> [Pos] -> [Pos]

-- | guard is basically declaration form of if expression
basin _ pos acc | pos `elem` acc = acc
basin grid pos acc = foldr (basin grid) (pos : acc) basinNeighbors
  where
    neighbors = [add pos d | d <- adjacencies]
    -- /= is not equals
    basinNeighbors = [p | p <- neighbors, at grid p /= 9]

-- more complicated variant (unused)
-- >>> basin2 sampleGrid [] [(1, 2)]
-- [(2,1),(1,1),(1,2)]
basin2 _ set [] = set
basin2 grid set (pos : frontier) | pos `elem` set = basin2 grid set frontier
basin2 grid set (pos : frontier) = basin2 grid (pos : set) (basinNeighbors ++ frontier)
  where
    neighbors = [add pos d | d <- adjacencies]
    basinNeighbors = [p | p <- neighbors, at grid p /= 9, p `notElem` set]

main :: IO ()
main = do
  grid <- pad . map parseLine . lines <$> readFile "input"
  let width = length (head grid) - 2
  let height = length grid - 2
  let lowPoints = [(r, c) | r <- [1 .. height], c <- [1 .. width], isLowPoint grid (r, c)]
  print $ sum [(grid !! r !! c) + 1 | (r, c) <- lowPoints]
  let basinSizes = map (length . (\p -> basin grid p [])) lowPoints
  -- sortOn is *actually* like Python's sort by key
  -- it is equivalent to sortBy (comparing f)
  print $ product $ take 3 . sortOn negate $ basinSizes
