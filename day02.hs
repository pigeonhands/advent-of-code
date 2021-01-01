import Data.List

-- inputData has been trimmed for file size sake
inputData :: [((Int,Int), Char, String)]
inputData = [((6,10), 's', "snkscgszxsssscss"),((6,7), 'b', "bbbbbxkb"),((2,4), 'n', "nnnjn"),((1,2), 'j', "jjjj"),((5,9), 'z', "jgzzzqhbj"),((4,11), 'm', "mfmmmpmjmkdr"),((12,15), 't', "twqrxttwttthtkxbz"),((8,9), 'z', "ftzjzzzzr"),((17,18), 'h', "cpkhssvpphzvprfnft"),((7,8), 'b', "bjbbbbbb"),((4,5), 'p', "pppppppppgppps"),((16,18), 'r', "rrrrrrrrrrrrrrrrrr"),((9,16), 'v', "vvvrpvbvvvvvvvvvwvvh"),((11,15), 'd', "ddddddddddddddjd"),((9,14), 'g', "ggbggghggggggggw"),((1,5), 'd', "ddddbd"),((1,4), 'x', "xxxwxxx"),((1,2), 'l', "bdjjddlqg"),((1,4), 'b', "lbbxb"),((15,16), 'f', "ffffffffffffffmz"),((10,16), 'm', "mmlmmmvmmmbhmmmq"),((4,15), 'v', "vlfvvqphhjfvlgt"),((5,12), 'm', "mmmmjmwmmmmcmmm"),((7,8), 'q', "qqqqqqxkq"),((4,9), 'h', "hhzhhhhhhhhhsh"),((3,7), 't', "thltdtjtstzrtwtt"),((6,7), 'k', "kkkkkkk"),((1,5), 'q', "jqwqd"),((4,13), 'x', "xxxxxxxxxxxxxxxxxx"),((1,4), 'l', "llfl"),((6,12), 'n', "nnffnfnnmnffnx"),((4,6), 'm', "xmvxnmpmm"),((5,7), 'm', "mmmmbmmmmmmmmm"),((9,17), 'f', "ffffflffbfffffcffff"),((4,10), 'k', "kkklkkkkkhktkkbkzq"),((8,15), 'z', "kdxxzzlhpzgbzjzz"),((2,5), 'q', "qrqqbqqqqqqqkqq"),((3,5), 't', "zrttht"),((9,12), 't', "ttxgntjmvntctpfrt"),((2,3), 'k', "kkkk"),((8,10), 'j', "jjjjjjjjjj"),((2,9), 'k', "vkwkhcqnk"),((9,10), 't', "ttttttthtt"),((4,6), 'b', "bbbbbbbb"),((9,12), 'n', "xnvnnvldhthlsn"),((2,4), 'w', "wwwwwwj"),((6,10), 't', "tttttwttttvtt"),((3,10), 'j', "jqjjjxjdjnjjjj"),((15,18), 'q', "kqlncdqwclqpjzrbnq"),((7,8), 'p', "gpwbjppp"),((3,13), 'm', "mmmmlsmfvmhmmmm"),((7,10), 's', "wdshsrsgsl"),((8,16), 'f', "fffffffxfffffffcf"),((16,18), 's', "ssskswhsvslwsssrsq"),((12,14), 'j', "gjjjjkjgjkjhjvj"),((13,14), 't', "gtttvftwtgvhlt"),((6,7), 'v', "vvvvvgbv"),((2,8), 'l', "ssldslmvl")]


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

validPasswords :: (Int -> Int -> Char -> String -> Bool) -> [String]
validPasswords f = [p | ((x1,x2), c, p) <- inputData, f x1 x2 c p]

policy1PasswordIsValid :: Int -> Int -> Char -> String -> Bool
policy1PasswordIsValid min max c p = isBetweenInc min max $ occurancesOfChar c p

part1 :: Int
part1 = length $ validPasswords policy1PasswordIsValid

policy2PasswordIsValid :: Int -> Int -> Char -> String -> Bool
policy2PasswordIsValid i1 i2 c p = (length $ intersect [i1-1,i2-1] $ indexesOfChar 0 [] c p) == 1

part2 :: Int
part2 = length $ validPasswords policy2PasswordIsValid