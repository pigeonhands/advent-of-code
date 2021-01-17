readInput :: IO [String]
readInput = do
    content <- readFile "day03.txt"
    return $ lines $ filter (not . (`elem` "\r")) content


flightPath :: [String] -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
flightPath fm (x,y) s@(sx,sy)
    | y >= length fm = []
    | otherwise = (x,y):flightPath fm (x + sx, y + sy) s

isTree :: [String] -> (Int, Int) -> Bool
isTree fm (x, y)
    | y >= length fm = False
    | otherwise =  row !! mod x (length row) == '#'
    where row = fm!!y

treesInPath :: [String] -> (Int,Int) -> [(Int,Int)]
treesInPath fm (sx,sy) = [p | p <- flightPath fm (0,0) (sx, sy), isTree fm p]

part1 :: IO Int
part1 = do
    flightMap <- readInput
    return $ length $ treesInPath flightMap (3,1)

part2 :: IO Int
part2 = do
    flightMap <- readInput
    return $ product $ map (length . treesInPath flightMap) [(1,1), (3,1), (5,1), (7,1), (1,2)]