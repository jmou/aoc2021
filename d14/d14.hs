{-# LANGUAGE TupleSections #-}

import Data.List (group, sort)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MultiSet

type Element = Char

type Pair = (Element, Element)

type Insertions = M.Map Pair Element

sample = "NNCB\n\nCH -> B\nHH -> N\nCB -> H\nNH -> C\nHB -> C\nHC -> B\nHN -> C\nNN -> C\nBH -> H\nNC -> B\nNB -> B\nBN -> B\nBB -> N\nBC -> B\nCC -> N\nCN -> C\n"

(sampleTemplate, sampleInsertions) = either error id $ parse sample

-- >>> length $ sampleIterations !! 5
-- 97
-- >>> length $ sampleIterations !! 10
-- 3073
sampleIterations = iterate (step sampleInsertions) sampleTemplate

parse = parse' . lines
  where
    parse' (template : "" : xs) = (template,) <$> parseInsertions xs
    parse' _ = Left "empty input"
    parseInsertions [] = Right M.empty
    parseInsertions ([a, b, ' ', '-', '>', ' ', c] : xs) = M.insert (a, b) c <$> parseInsertions xs
    parseInsertions _ = Left "bad insertion"

-- >>> step sampleInsertions sampleTemplate
-- "NCNBCHB"
step _ "" = ""
step _ [a] = [a]
step insertions (a : b : rest) = a : maybeToList (M.lookup (a, b) insertions) ++ step insertions (b : rest)

-- >>> spread $ sampleIterations !! 10
-- 1588
spread xs = most - least
  where
    grouped = map (\xs -> (length xs, head xs)) $ group . sort $ xs
    (most, _) = maximum grouped
    (least, _) = minimum grouped

-- >>> bag sampleTemplate
-- fromOccurList [(('B','_'),1),(('C','B'),1),(('N','C'),1),(('N','N'),1),(('_','N'),1)]
bag :: String -> MultiSet Pair
bag xs = bag' ('_' : xs)
  where
    bag' [] = MultiSet.empty
    bag' [x] = MultiSet.singleton (x, '_')
    bag' (x1 : x2 : xs) = MultiSet.insert (x1, x2) $ bag' (x2 : xs)

-- >>> count $ bag sampleTemplate
-- fromOccurList [('B',1),('C',1),('N',2)]
count :: MultiSet Pair -> MultiSet Element
-- concatMap is equivalent to but more efficient than concat . map
-- it is also similar to bind; in fact List.concatMap is equivalent to flip bind (=<<)
-- Set has no Monad instance because the type system can't reconcile Set's Ord constraint
count = MultiSet.delete '_' . MultiSet.foldOccur halve MultiSet.empty . MultiSet.concatMap toList
  where
    halve x n = MultiSet.insertMany x (n `div` 2)
    toList (a, b) = [a, b]

-- efficient implementation with MultiSet bags instead of String
-- >>> count $ step' sampleInsertions $ bag sampleTemplate
-- fromOccurList [('B',2),('C',2),('H',1),('N',2)]
-- >>> length . count $ iterate (step' sampleInsertions) (bag sampleTemplate) !! 5
-- 97
-- >>> count $ iterate (step' sampleInsertions) (bag sampleTemplate) !! 10
-- fromOccurList [('B',1749),('C',298),('H',161),('N',865)]
step' :: Insertions -> MultiSet Pair -> MultiSet Pair
step' insertions = MultiSet.concatMap replacePair
  where
    replacePair (a, b) = case M.lookup (a, b) insertions of
      Nothing -> [(a, b)]
      Just c -> [(a, c), (c, b)]

-- >>> spread' . count $ iterate (step' sampleInsertions) (bag sampleTemplate) !! 10
-- 1588
spread' :: MultiSet Element -> Int
spread' elements = most - least
  where
    grouped = (\(x, n) -> (n, x)) <$> MultiSet.toOccurList elements
    (most, _) = maximum grouped
    (least, _) = minimum grouped

main = do
  input <- readFile "input"
  let (template, insertions) = either error id $ parse input
  print $ spread $ iterate (step insertions) template !! 10
  print $ spread' . count $ iterate (step' insertions) (bag template) !! 40
