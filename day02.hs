import Data.List
import Data.List.Split

parseLine :: String -> ((Int,Int), Char, String)
parseLine l = ((r1,r2), (head c), ps)
    where (rs:c:ps:[]) = words l
          [r1,r2] = map read $ splitOn  "-" rs

readInput :: IO [((Int,Int), Char, String)]
readInput = do
    content <- readFile "day02.txt"
    return $ map parseLine $ lines content

indexesOfChar :: Num c => c -> [c] -> Char -> String -> [c]
indexesOfChar i ix c (x:xs)  
    | x == c    = indexesOfChar (i+1) (i:ix) c xs
    | otherwise = indexesOfChar (i+1) ix c xs 
indexesOfChar i ix n c = ix

occurancesOfChar :: Char -> String -> Int
occurancesOfChar c p = length $ indexesOfChar 0 [] c p

isBetweenInc :: Int -> Int -> Int -> Bool
isBetweenInc min max n 
    | n <= max && n >= min  = True
    | otherwise             = False

validPasswords :: [((Int,Int), Char, String)] -> (Int -> Int -> Char -> String -> Bool) -> [String]
validPasswords ix f = [p | ((x1,x2), c, p) <- ix, f x1 x2 c p]

policy1PasswordIsValid :: Int -> Int -> Char -> String -> Bool
policy1PasswordIsValid min max c p = isBetweenInc min max $ occurancesOfChar c p

part1 :: IO Int
part1 = do
    inputData <- readInput
    return $ length $ validPasswords inputData policy1PasswordIsValid

policy2PasswordIsValid :: Int -> Int -> Char -> String -> Bool
policy2PasswordIsValid i1 i2 c p = (length $ intersect [i1-1,i2-1] $ indexesOfChar 0 [] c p) == 1

part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ length $ validPasswords inputData policy2PasswordIsValid