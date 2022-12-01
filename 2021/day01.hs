import Data.List

readInput :: IO [Int]
readInput = do
    content <- readFile "day01.txt"
    return $ map read $ lines content


groupsOf2 :: Num a => [a] -> [[a]]
groupsOf2 xs = [[x,y] | x:ys <- tails xs, y <- ys]

groupsOf3 :: Num a => [a] -> [[a]]
groupsOf3 xs = [[x,y,z] | x:ys <- tails xs, y:zs <- tails ys, z <- zs]

findSubsetEquals :: (Eq a, Num a) => ([a]->a) -> [[a]] -> a -> [a]
findSubsetEquals f (x:xs) n
            | f x == n = x
            | otherwise  = findSubsetEquals f xs n

part1 :: IO Int
part1 = do
    inputData <- readInput
    return $ product $ findSubsetEquals sum (groupsOf2 inputData) 2020

part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ product $ findSubsetEquals sum (groupsOf3 inputData) 2020

