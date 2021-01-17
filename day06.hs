import Data.List


readInput :: IO [String]
readInput = do
    content <- readFile "day06.txt"
    return $ lines $ filter (not . (`elem` "\r")) content

groupVotes :: [String] -> [[String]]
groupVotes = go []
    where
        go cs [] = [cs]
        go cs (x:xs) 
            | x == "" = cs:go [] xs
            | otherwise = go (x:cs) xs

uniqueGroupVotes :: [String] -> [String]
uniqueGroupVotes = map (nub . concat) . groupVotes

part1 :: IO Int
part1 = sum . map length . uniqueGroupVotes <$> readInput


part2 :: IO Int
part2 = sum . map (length . foldl1 intersect) . groupVotes <$> readInput
