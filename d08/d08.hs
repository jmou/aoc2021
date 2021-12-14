import Data.List (elemIndex, foldl', partition, permutations, sort)
import Data.Maybe (fromJust)
import Data.Set (fromList, toList)

type Digit = [Int]

type Puzzle = ([Digit], [Digit])

type Encoding = [Int]

sample = "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

-- Convert to Ints for easy indexing
norm :: String -> Digit
norm = sort . map (fromJust . (`elemIndex` "abcdefg"))

-- Correct digit segments in numerical order
-- >>> digits
-- [[0,1,2,4,5,6],[2,5],[0,2,3,4,6],[0,2,3,5,6],[1,2,3,5],[0,1,3,5,6],[0,1,3,4,5,6],[0,2,5],[0,1,2,3,4,5,6],[0,1,2,3,5,6]]
digits :: [Digit]
digits = norm <$> words "abcefg cf acdeg acdfg bcdf abdfg abdefg acf abcdefg abcdfg"

-- >>> parseLine sample
-- ([[0,1,2,3,4,5,6],[1,2,3,4,5],[0,2,3,5,6],[0,1,2,3,5],[0,1,3],[0,1,2,3,4,5],[1,2,3,4,5,6],[0,1,4,5],[0,1,2,3,4,6],[0,1]],[[1,2,3,4,5],[0,1,2,3,5],[1,2,3,4,5],[0,1,2,3,5]])
parseLine :: String -> Puzzle
parseLine = cleave . words
  where
    -- split input on "|" and normalize
    cleave [] = error "no |"
    cleave ("|" : xs) = ([], map norm xs)
    cleave (x : xs) = let (l, r) = cleave xs in (norm x : l, r)

-- concatMap is like Javascript flatMap?
part1 = length . filter (`elem` [2, 4, 3, 7]) . map length . concatMap snd

rewire :: Encoding -> Digit -> Digit
-- !! is list index
rewire segments = sort . map (segments !!)

-- >>> brute $ parseLine sample
-- [2,5,6,0,1,3,4]
brute :: Puzzle -> Encoding
brute puzzle =
  -- brute force every permutation until we find one that matches the correct digit segments
  head
    [ segments | segments <- permutations [0 .. 6], (== sort digits) . sort $ rewire segments <$> unique
    ]
  where
    -- toList . fromList dedupes through Data.Set
    unique = toList . fromList $ uncurry (++) puzzle

-- >>> decode (parseLine sample) [2,5,6,0,1,3,4]
-- 5353
decode :: Puzzle -> Encoding -> Int
decode puzzle segments = foldl' (\n d -> n * 10 + d) 0 $ convert (snd puzzle)
  where
    -- fromJust extracts from Just or errors if Nothing
    convert = map (fromJust . (`elemIndex` digits) . rewire segments)

-- >>> part2 [parseLine sample]
-- 5353
part2 input = sum $ map (\p -> decode p (brute p)) input

main = do
  input <- map parseLine . lines <$> readFile "input"
  print $ part1 input
  print $ part2 input
