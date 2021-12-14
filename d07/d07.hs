sample = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

-- >>> align id sample
-- 37
-- >>> align cost sample
-- 168
align :: Integral a => (a -> a) -> [a] -> a
align f xs = minimum $ map cum [lo .. hi]
  where
    lo = minimum xs
    hi = maximum xs
    cum pos = sum . map (f . abs . (pos -)) $ xs

-- >>> map cost [0..5]
-- [0,1,3,6,10,15]
cost d = (d + 1) `div` 2 * (d + (d + 1) `rem` 2)

main = do
  contents <- readFile "input"
  let input = read ("[" ++ contents ++ "]") :: [Int]
  print $ align id input
  print $ align cost input
