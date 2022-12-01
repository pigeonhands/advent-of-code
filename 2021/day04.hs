import Data.Char
import Data.List
import Data.List.Split


parseSets' :: [[(String, String)]] -> [(String, String)] -> [String] -> [[(String, String)]]
parseSets' ss cs [] = cs:ss
parseSets' ss cs (x:xs)
    | x == "" = parseSets' (cs:ss) [] xs
    | otherwise = parseSets' ss ((n,v):cs) xs
    where [n,v] = splitOn ":" x

parseSets :: [String] -> [[(String, String)]]
parseSets = go []
    where 
        go cs [] = [cs]
        go cs (x:xs)
            | x == "" = cs:go [] xs
            | otherwise = go ((n,v):cs) xs
            where [n,v] = splitOn ":" x

readInput :: IO [[(String, String)]]
readInput = do
    content <- readFile "day04.txt"
    return . parseSets $ splitOneOf "\n " $ filter (not . (`elem` "\r")) content

filterFields :: [String] -> [(String, String)] -> [(String, String)]
filterFields fs xs = [(f,v) | (f,v) <- xs, f `elem` fs]

fieldIsValid :: (String, String) -> Bool
fieldIsValid (f,v) = 
    case f of "byr" -> vi >= 1920 && vi <= 2002
              "iyr" -> vi >= 2010 && vi <= 2020
              "eyr" -> vi >= 2020 && vi <= 2030
              "hgt" -> hightIsValid
              "hcl" -> (length v == 7) && (head v == '#') && all isAlphaNum (drop 1 v)
              "ecl" -> v `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
              "pid" -> length v == 9
              _ -> False
    where vi = read v
          hVal = read $ take (length v - 2) v
          hightIsValid = case reverse . take 2 $ reverse v of 
                    "cm" -> hVal >= 150 && hVal <= 193
                    "in" -> hVal >= 59 && hVal <= 76
                    _ -> False

requiredFields :: [String]
requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

part1 :: IO Int
part1 = do
    inputData <- readInput
    return $ length [x | x <- map (length . filterFields requiredFields) inputData, x == length requiredFields]

part2 :: IO Int
part2 = do
    inputData <- readInput
    return $ length [x | x <- inputData, length (filter fieldIsValid x) == length requiredFields]