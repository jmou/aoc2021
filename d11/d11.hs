-- quite awkward. some things to look into:
-- Array https://www.haskell.org/tutorial/arrays.html https://www.reddit.com/r/adventofcode/comments/rds32p/comment/ho65cui/
-- State Monad https://www.reddit.com/r/adventofcode/comments/rds32p/comment/ho6uygw/
-- software transactional vector? https://www.reddit.com/r/adventofcode/comments/rds32p/comment/ho9zrv8/

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.List.NonEmpty (groupWith, toList)
import Data.Maybe (fromJust)

sample = ["5483143223", "2745854711", "5264556173", "6141336146", "6357385478", "4167524645", "2176841721", "6882881134", "4846848554", "5283751526"]

sampleGrid = pad . map parseLine $ sample

-- record type; basically just gives convenient accessors to fields
data Grid = Grid {cells :: [Int], width :: Int, height :: Int} deriving (Show)

-- apparently repurposing Show for pretty printing is not advisable
-- https://www.stephendiehl.com/posts/strings.html
-- instance Show Grid where
--   showsPrec _ (Grid cells width _) = showRow $ drop (width + 2) cells
--     where
--       showRow [] s = s
--       showRow cells s =
--         if null rest then s else showString rowString restString
--         where
--           (row, rest) = splitAt (width + 2) cells
--           rowString = concatMap show (tail . init $ row)
--           restString = showChar '\n' (showRow rest s)

newtype Pos = Pos (Int, Int) deriving (Show)

at :: Grid -> Pos -> Int
at grid (Pos (r, c)) = cells grid !! ((width grid + 2) * (r + 1) + (c + 1))

add :: Pos -> Pos -> Pos
add (Pos (a, b)) (Pos (x, y)) = Pos (a + x, b + y)

row :: Pos -> Int
row (Pos (r, _)) = r

flashes :: Grid -> Int
flashes (Grid cells _ _) = length . filter (== 0) $ cells

allFlashed :: Grid -> Bool
allFlashed grid@(Grid _ width height) = flashes grid == width * height

-- awkwardly update Grid's cells
update grid cells = Grid cells (width grid) (height grid)

parseLine :: String -> [Int]
-- alternatively was using the esoteric (read . (: []))
parseLine = map digitToInt

-- >>> pad . map parseLine $ sample
-- Grid {cells = [20,20,20,20,20,20,20,20,20,20,20,20,20,5,4,8,3,1,4,3,2,2,3,20,20,2,7,4,5,8,5,4,7,1,1,20,20,5,2,6,4,5,5,6,1,7,3,20,20,6,1,4,1,3,3,6,1,4,6,20,20,6,3,5,7,3,8,5,4,7,8,20,20,4,1,6,7,5,2,4,6,4,5,20,20,2,1,7,6,8,4,1,7,2,1,20,20,6,8,8,2,8,8,1,1,3,4,20,20,4,8,4,6,8,4,8,5,5,4,20,20,5,2,8,3,7,5,1,5,2,6,20,20,20,20,20,20,20,20,20,20,20,20,20], width = 10, height = 10}
pad :: [[Int]] -> Grid
pad rows@(top : _) = Grid cells width height
  where
    width = length top
    height = length rows
    sentinel = 20
    topBot = replicate (width + 2) sentinel
    padLeftRight row = [sentinel] ++ row ++ [sentinel]
    cells = concat ([topBot] ++ map padLeftRight rows ++ [topBot])
pad [] = error "empty input"

-- >>> adjacencies
-- [Pos (-1,-1),Pos (-1,0),Pos (-1,1),Pos (0,-1),Pos (0,1),Pos (1,-1),Pos (1,0),Pos (1,1)]
adjacencies :: [Pos]
adjacencies = [Pos (r, c) | r <- [-1 .. 1], c <- [-1 .. 1], (r, c) /= (0, 0)]

-- recursively flash until no cells are flashing
-- >>> flash $ update sampleGrid (map (+ 2) (cells sampleGrid))
-- Grid {cells = [20,20,20,20,20,20,20,20,20,20,20,20,20,8,8,0,7,4,7,6,5,5,5,20,20,5,0,8,9,0,8,7,0,5,4,20,20,8,5,9,7,8,8,9,6,0,8,20,20,8,4,8,5,7,6,9,6,0,0,20,20,8,7,0,0,9,0,8,8,0,0,20,20,6,6,0,0,0,8,8,9,8,9,20,20,6,8,0,0,0,0,5,9,4,3,20,20,0,0,0,0,0,0,7,4,5,6,20,20,9,0,0,0,0,0,0,8,7,6,20,20,8,7,0,0,0,0,6,8,4,8,20,20,20,20,20,20,20,20,20,20,20,20,20], width = 10, height = 10}
flash grid = if 10 `elem` cells grid then flash flashed else reset
  where
    -- groupWith is like grouping by a key
    -- it returns NonEmpty lists, which are mostly like normal []
    positions = groupWith row [Pos (r, c) | r <- [0 .. height grid - 1], c <- [0 .. width grid - 1]]
    evolve pos = case grid `at` pos of
      11 -> 11 -- sentinel for flashed
      10 -> 11 -- sentinel for flashing
      -- calculate increment from flashing neighbors, capped at 10 (flashing)
      e -> min 10 (e + length [1 | d <- adjacencies, grid `at` (pos `add` d) == 10])
    -- must use fmap on NonEmpty, since map is specialized for []
    -- toList turns NonEmpty into []
    flashed = pad $ toList . fmap evolve <$> positions
    -- drain energy from flashed cells
    reset = pad $ toList . fmap ((\e -> if e == 11 then 0 else e) . (grid `at`)) <$> positions

-- >>> iterate step sampleGrid !! 100
-- Grid {cells = [20,20,20,20,20,20,20,20,20,20,20,20,20,0,3,9,7,6,6,6,8,6,6,20,20,0,7,4,9,7,6,6,9,1,8,20,20,0,0,5,3,9,7,6,9,3,3,20,20,0,0,0,4,2,9,7,8,2,2,20,20,0,0,0,4,2,2,9,8,9,2,20,20,0,0,5,3,2,2,2,8,7,7,20,20,0,5,3,2,2,2,2,9,6,6,20,20,9,3,2,2,2,2,8,9,6,6,20,20,7,9,2,2,2,8,6,8,6,6,20,20,6,7,8,9,9,9,8,7,6,6,20,20,20,20,20,20,20,20,20,20,20,20,20], width = 10, height = 10}
step grid = flash incremented
  where
    incremented = update grid (map (+ 1) (cells grid))

-- >>> sum . take 100 . drop 1 $ flashes <$> iterate step sampleGrid
-- 1656
-- >>> fromJust . (True `elemIndex`) $ allFlashed <$> iterate step sampleGrid
-- 195
main = do
  input <- lines <$> readFile "input"
  let grid = pad . map parseLine $ input
      -- iterate makes an infinite list; rely on lazy list evaluation
      steps = iterate step grid
  print $ sum . take 100 . drop 1 $ flashes <$> steps
  -- print $ fromJust . (True `elemIndex`) $ allFlashed <$> iterate step grid
  print $ length $ takeWhile (not . allFlashed) steps
