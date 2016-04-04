import qualified Data.Text as T

paperSize :: [Integer] -> Integer
paperSize [l, w, h] = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]

ribbonSize :: [Integer] -> Integer
ribbonSize [l, w, h] = 2 * (minimum [l+w, w+h, h+l]) + l*w*h

parseDims :: String -> [Integer]
parseDims s = 
    map read $ map T.unpack $ T.splitOn (T.pack "x") (T.pack s)

compute :: ([Integer] -> Integer) -> [String] -> Integer
compute f boxes =
    sum $ map f $ map parseDims boxes


main = do
    x <- lines <$> getContents
    print $ compute paperSize x
    print $ compute ribbonSize x
