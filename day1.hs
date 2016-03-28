
parseSequence floor string =
    case string of
        ('(':t) -> parseSequence (floor + 1) t
        (')':t) -> parseSequence (floor - 1) t
        otherwise -> floor

main = do
    s <- getLine
    let floor = parseSequence 0 s
    print floor 

