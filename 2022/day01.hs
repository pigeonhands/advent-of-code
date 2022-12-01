import Data.List

readInput :: IO [String]
readInput = do
    content <- readFile "day01.txt"
    return $ lines content

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left,right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list

maxItem  :: [Int] -> Int
maxItem (x:xs) = go x xs
    where
        go a [] = a
        go a [b]
            | a > b  = a
            | otherwise = b
        go a (x:xs) = go maxVal xs
            where maxVal = maxItem [a, x]

calorieList :: [String] -> [Int]
calorieList = (map sum) . map (map read) . splitList ""

part1 :: IO Int
part1 = do
    inputData <- readInput
    return $ maxItem . calorieList $ inputData

part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ sum . take 3 . reverse . sort . calorieList $ inputData