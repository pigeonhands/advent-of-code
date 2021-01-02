import Data.List


readInput :: IO [String]
readInput = do
    content <- readFile "day06.txt"
    return $ lines $ filter (not . (`elem` "\r")) content

groupVotes' :: [[String]] -> [String] -> [String] -> [[String]]
groupVotes' ls cs [] = cs:ls
groupVotes' ls cs (x:xs) 
    | x == "" = groupVotes' (cs:ls) [] xs
    | otherwise = groupVotes' ls (x:cs) xs

groupVotes :: [String] -> [[String]]
groupVotes xs = groupVotes' [] [] xs

uniqueGroupVotes :: [String] -> [String]
uniqueGroupVotes = map (nub . concat) . groupVotes

part1 :: IO Int
part1 = do
    inputData <- readInput
    return $ sum $ map length (uniqueGroupVotes inputData)


part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ sum $ map (length . foldl1 intersect) $ groupVotes inputData
