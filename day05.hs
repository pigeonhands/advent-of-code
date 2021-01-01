import Data.List

inputData :: [String]
inputData = [
    "FBBBBBBRRL",
    "BBFFFBBLRL",
    "FBFFBFFLRL",
    "FFBBBFBLRR",
    "FFBFBFFLLL",
    "BFBBBBFLLL"]

getAggFunction :: Char -> Char -> Char -> (Int -> [a] -> [a])
getAggFunction uc lc ic
    | lc == ic = take
    | uc == ic = drop

findBinary ::  Char -> Char -> [Int] -> String -> Int
findBinary uc lc (r1:r2:[]) (x:[])
    | x == lc = r1
    | otherwise = r2
findBinary uc lc rs (x:xs) = 
    findBinary uc lc (bf half rs) xs
    where 
        half = ((length rs) `div` 2)
        bf = getAggFunction uc lc x

findRow :: [Int] -> String -> Int
findRow = findBinary 'B' 'F'

findCol :: [Int] -> String -> Int
findCol = findBinary 'R' 'L'

toSeatID :: String -> Int
toSeatID ls = (row * 8) + col
    where 
        (rs,cs) = splitAt 7 ls
        row = findRow [0..128] rs
        col = findCol [0..7] cs

findSeat :: String -> (String,String)
findSeat s = (cs,rs)
    where (cs,rs) = splitAt 7 s

part1 :: Int
part1 = maximum $ map toSeatID inputData

findNonSequentialNumber :: [Int] -> Int
findNonSequentialNumber (x:xs)
    | (x+1) == head xs = findNonSequentialNumber xs
    | otherwise = x+1

part2 :: Int
part2 = findNonSequentialNumber $ sort $ map toSeatID inputData