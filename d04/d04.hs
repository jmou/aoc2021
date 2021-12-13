import Data.List (partition, scanl', transpose)

-- newtypes wrap existing types; could have used a type alias instead
newtype Board = Board [[Int]] deriving (Read, Show)

-- base provides this for Text, but this generalizes to split paragraphs
splitOn :: Eq a => a -> [a] -> [[a]]
-- partially applied == infix is called a section
-- break splits a list in two where the predicate is true
splitOn delim full = go $ break (== delim) full
  where
    go (x, []) = [x]
    go (x, delim : y) = x : splitOn delim y

-- transpose to also check winning columns
winning drawn (Board rows) = any winningRow (rows ++ transpose rows)
  where
    -- elem is like `in` (or the flip of contains)
    winningRow row = all (`elem` drawn) row

score :: [Int] -> Board -> Int
score drawn (Board rows) = sum unmarked * head drawn
  where
    unmarked = filter (not . (`elem` drawn)) (concat rows)

winOrder :: [[Int]] -> [Board] -> [([Int], Board)]
winOrder [] _ = error "unwon boards"
winOrder _ [] = []
winOrder (drawn : drawings) boards =
  -- (,) makes an infix into a prefix; in this case we partially apply it
  -- alternatively could use TupleSections pragma
  map ((,) drawn) winners ++ winOrder drawings losers
  where
    -- partition separates by a predicate
    (winners, losers) = partition (winning drawn) boards

main = do
  (drawLine : "" : boardLines) <- lines <$> readFile "input"
  let draws = read <$> splitOn ',' drawLine :: [Int]
  -- nested maps for boards > lines > words
  let boards = Board . map (map read . words) <$> splitOn [] boardLines
  -- flip applies a curried function with two arguments in reverse
  -- scanl' is a strict variant of scanl
  -- scanl is like foldl, but it returns a list of each intermediate
  -- first 5 can't win so skip them
  let drawings = drop 5 $ scanl' (flip (:)) [] draws
  let order = winOrder drawings boards
  -- uncurry applies a curried function to a pair
  print $ uncurry score $ head order
  print $ uncurry score $ last order
