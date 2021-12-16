-- `$ stack d12.hs`
-- stack run also works but is heavier; maybe needed for package dependencies

import Data.Char (isUpper)
import Data.List (sort)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)

type System = M.Map Cave [Cave]

type Cave = String

type Path = [Cave]

sample = ["start-A", "start-b", "A-c", "A-b", "b-d", "A-end", "b-end"]

-- >>> parse "foo-bar"
-- ("foo","bar")
parse :: String -> (Cave, Cave)
-- break partitions once but leaves the delimiter
-- <$> fmap seems to operate on snd of a tuple
parse line = tail <$> break (== '-') line

-- >>> load $ map parse sample
-- fromList [("A",["b","c","end","start"]),("b",["A","d","end","start"]),("c",["A"]),("d",["b"]),("end",["A","b"]),("start",["A","b"])]
load :: [(Cave, Cave)] -> System
-- I thought sorting may be helpful but it's not; left it in as an example
load caves = M.map sort $ foldr load' M.empty caves
  where
    load' (a, b) system =
      M.insertWith (++) a [b] $
        M.insertWith (++) b [a] system

-- >>> search 0 $ load [("start", "a"), ("start", "b"), ("a", "b"), ("b", "end")]
-- [["end","b","a","start"],["end","b","start"]]
-- >>> search 0 . load . map parse $ sample
-- [["end","A","c","A","b","A","start"],["end","A","b","A","start"],["end","b","A","start"],["end","A","b","A","c","A","start"],["end","b","A","c","A","start"],["end","A","c","A","start"],["end","A","start"],["end","A","c","A","b","start"],["end","A","b","start"],["end","b","start"]]
-- >>> length . search 0 . load . map parse $ sample
-- 10
-- >>> length . search 1 . load . map parse $ sample
-- 36
search :: Int -> System -> [Path]
search recrossings system = search' system [] ["start"] recrossings
  where
    search' :: System -> Path -> [Cave] -> Int -> [Path]
    search' _ _ [] _ = [] -- no next caves
    search' _ path@("end" : _) _ _ = [path] -- reached the end!
    search' system path (next : others) recrossings = nextPaths ++ othersPaths
      where
        searchNext = search' system (next : path) (fromJust $ M.lookup next system)
        nextPaths
          -- big cave or unvisited
          | all isUpper next || next `notElem` path = searchNext recrossings
          -- must be a little cave; not start and recrossing allowed
          | recrossings > 0 && next /= "start" = searchNext (recrossings - 1)
          | otherwise = []
        othersPaths = search' system path others recrossings

main = do
  input <- lines <$> readFile "input"
  let system = load . map parse $ input
  print $ length . search 0 $ system
  print $ length . search 1 $ system
