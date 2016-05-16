import Data.Set as Set

data House = House Integer Integer deriving (Show, Ord)
data HouseSet = HouseSet House (Set House) deriving Show

instance Eq House where
    (House x1 y1) == (House x2 y2) = x1 == x2 && y1 == y2

compare :: House -> House -> Ordering
compare (House x1 y1) (House x2 y2) = Prelude.compare (Prelude.compare x1 x2) (Prelude.compare y2 y2)

nextHouse :: Char -> House -> House
nextHouse '>' (House x y) = House (x + 1) y
nextHouse '<' (House x y) = House (x - 1) $ y
nextHouse '^' (House x y) = House x $ y + 1
nextHouse 'v' (House x y)  = House x $ y - 1
nextHouse c (House x y) = House x y

move :: Char -> HouseSet -> HouseSet
move c (HouseSet h s) = 
    let h' = nextHouse c h in HouseSet h' $ Set.insert h' s

emptyHouseSet = (HouseSet (House 0 0) Set.empty)

computeMoves :: [Char] -> Int
computeMoves moves =
    case Prelude.foldr move emptyHouseSet moves of
        (HouseSet h s) -> Set.size s

main = do
    contents <- getContents
    print $ computeMoves contents

