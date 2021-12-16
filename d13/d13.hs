-- these both behave a bit like Rust's if let
-- pattern guards allows pattern matching from guard expressions
{-# LANGUAGE PatternGuards #-}
-- view patterns are very similar but without guards
-- a "view" function's result can be pattern matched
-- https://neilmitchell.blogspot.com/2009/11/reviewing-view-patterns.html
{-# LANGUAGE ViewPatterns #-}

import Control.Arrow (first, second)
import Data.List (foldl', nub, stripPrefix)

sample = ["6,10", "0,14", "9,10", "0,3", "10,4", "4,11", "6,0", "6,12", "4,1", "0,13", "10,12", "3,4", "3,0", "8,4", "1,10", "2,14", "8,10", "9,0", "", "fold along y=7", "fold along x=5"]

type Dot = (Int, Int)

data Axis = X | Y deriving (Show)

data Fold = Fold Axis Int deriving (Show)

-- >>> parse sample
-- ([(6,10),(0,14),(9,10),(0,3),(10,4),(4,11),(6,0),(6,12),(4,1),(0,13),(10,12),(3,4),(3,0),(8,4),(1,10),(2,14),(8,10),(9,0)],[FoldY 7,FoldX 5])
parse :: [String] -> ([Dot], [Fold])
parse lines = parseDots lines
  where
    splitFirstComma = fmap tail . break (== ',')
    readPair (a, b) = (read a, read b)
    parseDots [] = error "empty input"
    parseDots ("" : lines) = ([], parseFolds lines)
    parseDots (line : lines) = (dot : dots, folds)
      where
        dot = readPair $ splitFirstComma line
        (dots, folds) = parseDots lines
    parseFolds [] = []
    parseFolds (line : lines)
      -- pattern guard
      | Just axis <- stripPrefix "fold along " line =
        parseAxis axis : parseFolds lines
      where
        -- view pattern
        parseAxis (stripPrefix "x=" -> Just x) = Fold X $ read x
        parseAxis (stripPrefix "y=" -> Just y) = Fold Y $ read y
        parseAxis _ = error "bad axis"
    parseFolds _ = error "bad fold"

-- >>> fold (FoldY 7) $ fst $ parse sample
-- [(6,4),(0,0),(9,4),(0,3),(10,4),(4,3),(6,0),(6,2),(4,1),(0,1),(10,2),(3,4),(3,0),(8,4),(1,4),(2,0),(9,0)]
fold :: Fold -> [Dot] -> [Dot]
-- nub (essence) deduplicates
fold (Fold axis l) dots = nub $ fold' (update axis) l dots
  where
    -- don't really understand arrows, but first maps over the first
    -- leaving the second unchanged
    -- I think it is a generalization of bifunctors
    update X = first
    -- and vice versa
    update Y = second
    mirror l d = if d > l then 2 * l - d else d
    fold' _ _ [] = []
    -- which maps (mirror l) over the first or second of dot
    fold' which l (dot : dots) = which (mirror l) dot : fold' which l dots

render :: [Dot] -> String
-- unlines is the inverse of lines; like intercalate "\n" (but at the end too)
render dots = unlines [[at x y | x <- [0 .. maxX]] | y <- [0 .. maxY]]
  where
    maxX = maximum . map fst $ dots
    maxY = maximum . map snd $ dots
    at x y = if (x, y) `elem` dots then '*' else ' '

main = do
  (dots, folds) <- parse . lines <$> readFile "input"
  print $ length $ fold (head folds) dots
  let final = foldl' (flip fold) dots folds
  putStr $ render final
