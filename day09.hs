import Data.List

readInput :: IO [Int]
readInput = do
    content <- readFile "day09.txt"
    return $ map read $ lines content


numberIsWeak :: [Int] -> Int -> Bool 
numberIsWeak xs n = n `notElem ` [x+y| x:ys <- tails xs, y <- ys]

getFirstWeakNumber :: Int -> [Int] -> Int  
getFirstWeakNumber n xs 
    | numberIsWeak = i
    | otherwise = getFirstWeakNumber n (tail xs)
    where set = take n xs
          i = last set
          numberIsWeak = i `notElem ` [x+y| x:ys <- tails (take (n-1) set), y <- ys]

getSumSequence :: Int -> [Int] -> [Int]
getSumSequence = go []
    where
        go [] n (x:xs) = go [x] n xs
        go cx n (x:xs)
            | csum == n = cx
            | csum > n = []
            | otherwise = go (x:cx) n xs
            where csum = sum cx 

findSumPattern :: Int -> [Int] -> [Int]
findSumPattern n xs = head [
    sq | ys <- tails xs, 
    let sq = getSumSequence n ys, 
    not (null sq)]

part1 :: IO Int 
part1 = getFirstWeakNumber 26 <$> readInput

part2 :: IO Int 
part2 = do 
    inputData <- readInput
    let pattern = findSumPattern (getFirstWeakNumber 26 inputData) inputData
    return $ minimum pattern + maximum pattern
