main = do
    -- with precedence: ((map parse) . lines) <$> (readFile "input")
    input <- map parse . lines <$> readFile "input" :: IO [(Action, Integer)]
    let (x, y) = foldl part1 (0, 0) input
    print $ x * y
    let (x, y, _) = foldl part2 (0, 0, 0) input
    print $ x * y

data Action = Forward | Down | Up

parse :: String -> (Action, Integer)
parse line =
    case words line of
        ["forward", num] -> (Forward, read num)
        ["down", num] -> (Down, read num)
        ["up", num] -> (Up, read num)
        _ -> error "parse"

part1 (x, y) (action, num) =
    case action of
        Forward -> (x + num, y)
        Down -> (x, y + num)
        Up -> (x, y - num)

part2 (x, y, aim) (action, num) =
    case action of
        Forward -> (x + num, y + aim * num, aim)
        Down -> (x, y, aim + num)
        Up -> (x, y, aim - num)
