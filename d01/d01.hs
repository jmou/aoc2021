-- $ stack --resolver ghc-8.10.5 script d01.hs

main = do
  -- :: type ascription is necessary to disambiguate read which parses a String
  -- . is function composition (f o g)
  -- <$> is fmap; it maps inside the functor (IO)
  -- <- is bind inside do-block (?)
  nums <- map read . lines <$> readFile "input" :: IO [Int]
  print $ countIncreases nums
  print $ countIncreases $ rollingSum nums

{-
https://github.com/haskell/haskell-language-server/blob/master/plugins/hls-eval-plugin/README.md

>>> countIncreases [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
7
-}
countIncreases nums =
  -- function application (just whitespace) has highest precedence
  -- . is function composition (f o g); $ would have worked here as well
  -- '$' is right associative lowest precedence function application
  -- (basically a noop to avoid lots of nested parentheses)
  -- zip stops at the end of the shortest list
  length . filter isIncrease $ zip nums (tail nums)
  where
    isIncrease (a, b) = b > a

-- >>> countIncreases $ rollingSum [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
-- 5
rollingSum nums =
  -- zipWith* takes a function to combine zipped elements
  zipWith3 addUp nums (drop 1 nums) (drop 2 nums)
  where
    addUp a b c = a + b + c
