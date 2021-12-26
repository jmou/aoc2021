import Data.Char (digitToInt, intToDigit)
import Data.Foldable (foldl')
import qualified Data.HashPSQ as PQ
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V

sample = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581"

data Grid = Grid {cells :: V.Vector Int, width :: Int} deriving (Show)

height grid = (V.length . cells) grid `div` width grid

grid ! (r, c) = cells grid V.! (r * width grid + c)

grid !!! (r, c) = manhattanWrap $ grid ! (r', c')
  where
    (m1, r') = r `divMod` height grid
    (m2, c') = c `divMod` width grid
    manhattanWrap x = (m1 + m2 + x - 1) `mod` 9 + 1

-- >>> display 2 $ parse sample
-- "11637517422274862853\n13813736722492484783\n21365113283247622439\n36949315694715142671\n74634171118574528222\n13191281372421239248\n13599124212461123532\n31254216394236532741\n12931385212314249632\n23119445813422155692\n22748628533385973964\n24924847833513595894\n32476224394358733541\n47151426715826253782\n85745282229685639333\n24212392483532341359\n24611235323572234643\n42365327415347643852\n23142496323425351743\n34221556924533266713\n"
display scale grid =
  T.concat
    [ T.snoc (T.pack [intToDigit $ grid !!! (r, c) | c <- [0 .. scale * width grid - 1]]) '\n'
      | r <- [0 .. scale * height grid - 1]
    ]

parse input = Grid (V.fromList . fmap digitToInt . concat $ inputLines) (length $ head inputLines)
  where
    inputLines = lines input

-- >>> shortest 1 $ parse sample
-- 40
-- >>> shortest 5 $ parse sample
-- 315
shortest :: Int -> Grid -> Int
shortest scale rm = dijkstra M.empty S.empty (PQ.singleton (0, 0) 0 ())
  where
    cMax = scale * width rm - 1
    rMax = scale * height rm - 1
    dijkstra dists visited frontier = if (r, c) == (rMax, cMax) then cost else dijkstra dists' visited' frontier''
      where
        Just ((r, c), cost, _, frontier') = PQ.minView frontier
        dists' = M.insertWith min (r, c) cost dists
        visited' = S.insert (r, c) visited
        neighbors = filter bounds [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
        bounds p'@(r', c') = S.notMember p' visited' && 0 <= r' && r' <= rMax && 0 <= c' && c' <= cMax
        frontier'' = foldl' (\q p -> snd $ PQ.alter (update (cost + rm !!! p)) p q) frontier' neighbors
        update cost' Nothing = ((), Just (cost', ()))
        update cost' (Just (p, v)) = ((), Just (min cost' p, v))

main = do
  parsed <- parse <$> readFile "input"
  print $ shortest 1 parsed
  print $ shortest 5 parsed
