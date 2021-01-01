import Data.List

inputData :: [String]
inputData = [
    "abc",
    "",
    "a",
    "b",
    "c",
    "",
    "ab",
    "ac",
    "",
    "a",
    "a",
    "a",
    "a",
    "",
    "b"]

groupVotes_ :: [[String]] -> [String] -> [String] -> [[String]]
groupVotes_ ls cs [] = cs:ls
groupVotes_ ls cs (x:xs) 
    | x == "" = groupVotes_ (cs:ls) [] xs
    | otherwise = groupVotes_ ls (x:cs) xs

groupVotes :: [[String]]
groupVotes = groupVotes_ [] [] inputData

uniqueGroupVotes :: [String]
uniqueGroupVotes = map (nub . concat) groupVotes

part1 :: Int
part1 = sum $ map length uniqueGroupVotes

part2 :: Int
part2 = sum $ map (length . foldl1 intersect) groupVotes
