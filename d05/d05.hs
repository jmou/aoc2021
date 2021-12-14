-- stack --resolver ghc-8.10.5 script --package text --package multiset
-- $ stack d05.hs

{-# LANGUAGE OverloadedStrings #-}

-- MultiSet are also known as bags
-- qualified imports need to be prefixed by MultiSet.
import qualified Data.MultiSet as MultiSet
import Data.Text (pack, splitOn, unpack)
import Numeric (readDec)

newtype Coord = Coord (Int, Int) deriving (Show, Eq, Ord)

-- Read typeclass parses from String; not entirely clear on it
instance Read Coord where
  readsPrec _ s =
    -- parsing in list comprehension generators seems idiomatic
    [ (Coord (x, y), u)
      | (x, ',' : t) <- readDec s,
        (y, u) <- readDec t
    ]

data Vent = Vent Coord Coord deriving (Show)

parseLine :: String -> Vent
parseLine line = Vent a b
  where
    -- pack converts String to Text, which has (kind of) built-in splitOn
    [a, b] = read . unpack <$> splitOn " -> " (pack line)

interpolate :: Int -> Int -> [Int]
interpolate a b
  -- .. desugars to enumFromTo
  | a < b = [a .. b]
  -- similarly desugars to enumFromThenTo
  | otherwise = [a,a-1..b]

countOverlapped :: (Vent -> [Coord]) -> [Vent] -> Int
countOverlapped interpolateCoord vents  =
  -- >>= is bind; not entirely clear but it's like flatmap
  let set = MultiSet.fromList (vents >>= interpolateCoord)
  -- foldOccur gives the element and its occurences
  in length $ MultiSet.foldOccur (\c o a -> if o > 1 then c : a else a) [] set

interpolateAxisAligned :: Vent -> [Coord]
interpolateAxisAligned (Vent (Coord (x1, y1)) (Coord (x2, y2)))
    -- both map over interpolated axis to construct Coords
    -- lambda function approach
  | x1 == x2 = (\y -> Coord (x1, y)) <$> interpolate y1 y2
    -- higher order functions
  | y1 == y2 = flip (curry Coord) y1 <$> interpolate x1 x2
  -- list comprehension
  -- | y1 == y2 = [Coord (x, y1) | x <- interpolate x1 x2]
  | otherwise = []

interpolate8Way :: Vent -> [Coord]
-- @ is "read as" for binding over a pattern match
interpolate8Way vent @ (Vent (Coord (x1, y1)) (Coord (x2, y2)))
  | abs (y2 - y1) == abs (x2 - x1) = Coord <$> zip (interpolate x1 x2) (interpolate y1 y2)
  | otherwise = interpolateAxisAligned vent

main = do
  vents <- map parseLine . lines <$> readFile "input"
  print $ countOverlapped interpolateAxisAligned vents
  print $ countOverlapped interpolate8Way vents
