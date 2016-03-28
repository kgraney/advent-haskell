-- Part 1
finalFloor floor string =
    case string of
        ('(':t) -> finalFloor (floor + 1) t
        (')':t) -> finalFloor (floor - 1) t
        [] -> floor

-- Part 2
enterBasement (-1) count string = count
enterBasement floor count string =
    case string of
        ('(':t) -> enterBasement (floor + 1) (count + 1) t
        (')':t) -> enterBasement (floor - 1) (count + 1) t
        [] -> -1

main = do
    s <- getLine
    print $ finalFloor 0 s
    print $ enterBasement 0 0 s
