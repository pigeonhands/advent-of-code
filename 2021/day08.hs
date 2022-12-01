parseParam :: String -> Int
parseParam all@(x:xs) 
    | x == '+' = read xs
    | otherwise = read all

parseLine :: Int -> String -> (Int, (String, Int))
parseLine ip l = (ip, (i, parseParam p))
    where [i,p] = words l
          
readInput :: IO [(Int, (String, Int))]
readInput = do
    content <- readFile "day08.txt"
    return $ zipWith parseLine [0 .. ] (lines content)

runInstruction :: (Int, Int) -> (String, Int) -> (Int, Int)
runInstruction (ip, acc) is = 
    case cmd of "nop" -> (ip+1, acc)
                "acc" -> (ip+1, acc+arg)
                "jmp" -> (ip+arg, acc)
    where (cmd,arg) = is

emulateCode :: [(Int, (String, Int))] -> (Int, Int)
emulateCode = go (0,0) []
    where
        go (ip, acc) sf isx 
            | ip `elem` sf = (ip, acc)
            | otherwise = case lookup ip isx of 
                Just ni -> go (runInstruction (ip, acc) ni) (ip:sf) isx
                Nothing -> (ip, acc)

swapInstruction :: Int -> [(Int, (String, Int))] -> [(Int, (String, Int))]
swapInstruction ip (c:cs)
    | ip == isp = (isp, (newc, arg)):cs
    | otherwise = c : swapInstruction ip cs
    where (isp, (cmd,arg)) = c
          newc = case cmd of 
                "nop" -> "jmp"
                "jmp" -> "nop"
    
part1 :: IO Int
part1 = do
    inputData <- readInput
    return . snd $ emulateCode inputData

part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ head [
        acc | tip <- [ip | (ip, (cmd,_)) <- inputData, cmd `elem` ["jmp", "nop"]], -- Get list of instruction indexes for jmp and nop instructions
        let (ip,acc) = emulateCode $ swapInstruction tip inputData, -- emulate each set of code with the instructons swapped
        ip == length inputData] -- Filter permutations to the ones that have an instruction pointer that is at the end of the code when completed