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
groupVotes = groupVotes' [] []

uniqueGroupVotes :: [String] -> [String]
uniqueGroupVotes = map (nub . concat) . groupVotes

part1 :: IO Int
part1 = sum . map length . uniqueGroupVotes <$> readInput


part2 :: IO Int
part2 = sum . map (length . foldl1 intersect) . groupVotes <$> readInput
