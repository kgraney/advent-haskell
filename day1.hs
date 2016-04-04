-- Part 1
finalFloor floor ('(':t) = finalFloor (floor + 1) t
finalFloor floor (')':t) = finalFloor (floor - 1) t
finalFloor floor [] = floor

-- Part 2
enterBasement (-1) count string = count
enterBasement floor count ('(':t) = enterBasement (floor + 1) (count + 1) t
enterBasement floor count (')':t) = enterBasement (floor - 1) (count + 1) t
enterBasement floor count [] = -1

main = do
    s <- getLine
    print $ finalFloor 0 s
    print $ enterBasement 0 0 s
