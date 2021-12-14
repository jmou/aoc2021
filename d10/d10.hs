import Data.Either (rights)
import Data.List (foldl', sort)

sample = ["[({(<(())[]>[[{[]{<()<>>", "[(()[<>])]({[<{<<[]>>(", "{([(<{}[<>[]}>{[]{[(<()>", "(((({<>}<{<{<>}{[]{[]{}", "[[<[([]))<([[{}[[()]]]", "[{[{({}]{}}([{[{{{}}([]", "{<[[]]>}<{[{[{[]{()[[[]", "[<(<(<(<{}))><([]([]()", "<{([([[(<>()){}]>(<<{{", "<{([{{}}[<[[[<>{}]]]>[]]"]

-- >>> map process sample
-- [Right "}}]])})]",Right ")}>]})",Left '}',Right "}}>}>))))",Left ')',Left ']',Right "]]}}]}]}>",Left ')',Left '>',Right "])}>"]
process :: [Char] -> Either Char [Char]
-- explicit tail recursion seems like a readable alternative to foldl'
process = foldl' reduce (Right [])
  where
    close '(' = Just ')'
    close '[' = Just ']'
    close '{' = Just '}'
    close '<' = Just '>'
    close _ = Nothing
    -- apparently foldM could return Left early
    reduce (Left illegal) _ = Left illegal
    reduce (Right stack) c = case close c of
      -- push expected close
      Just c' -> Right $ c' : stack
      Nothing -> case stack of
        -- pop matching close
        c' : cs | c == c' -> Right cs
        -- illegal character
        _ -> Left c

-- >>> part1 sample
-- 26397
part1 :: [[Char]] -> Int
part1 = sum . map (score . process)
  where
    score (Left ')') = 3
    score (Left ']') = 57
    score (Left '}') = 1197
    score (Left '>') = 25137
    score (Left _) = error "invalid char"
    score (Right _) = 0

-- >>> scoreIncomplete "}}]])})]"
-- 288957
scoreIncomplete :: [Char] -> Int
scoreIncomplete = foldl' (\score c -> score * 5 + points c) 0
  where
    points ')' = 1
    points ']' = 2
    points '}' = 3
    points '>' = 4
    points _ = error "invalid char"

median list = sort list !! (length list `div` 2)

-- >>> part2 sample
-- 288957
part2 :: [[Char]] -> Int
-- rights keeps only the Right values
-- alternatively could have used mapMaybe with scoreIncomplete accepting Either
part2 = median . map scoreIncomplete . rights . map process

main = do
  input <- lines <$> readFile "input"
  print $ part1 input
  print $ part2 input
