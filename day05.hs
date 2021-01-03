import Data.List

inputData :: [String]
inputData = [
    "FBBBBBBRRL",
    "BBFFFBBLRL",
    "FBFFBFFLRL",
    "FFBBBFBLRR",
    "FFBFBFFLLL",
    "BFBBBBFLLL"]

readInput :: IO [String]
readInput = do
    content <- readFile "day05.txt"
    return $ lines $ filter (not . (`elem` "\r")) content 

getAggFunction :: Char -> Char -> Char -> (Int -> [a] -> [a])
getAggFunction uc lc ic
    | lc == ic = take
    | uc == ic = drop

findBinary ::  Char -> Char -> [Int] -> String -> Int
findBinary uc lc [r1, r2] [x]
    | x == lc = r1
    | otherwise = r2
findBinary uc lc rs (x:xs) = 
    findBinary uc lc (bf half rs) xs
    where 
        half = length rs `div` 2
        bf = getAggFunction uc lc x

findRow :: String -> Int
findRow = findBinary 'B' 'F' [0..128]

findCol :: String -> Int
findCol = findBinary 'R' 'L' [0..7]

toSeatID :: String -> Int
toSeatID ls = (row * 8) + col
    where 
        (rs,cs) = splitAt 7 ls
        row = findRow rs
        col = findCol cs

findSeat :: String -> (String,String)
findSeat s = (cs,rs)
    where (cs,rs) = splitAt 7 s

part1 :: IO Int
part1 = maximum . map toSeatID <$> readInput

findNonSequentialNumber :: [Int] -> Int
findNonSequentialNumber (x:xs)
    | (x+1) == head xs = findNonSequentialNumber xs
    | otherwise = x+1

part2 :: IO Int
part2 = findNonSequentialNumber . sort . map toSeatID <$> readInput