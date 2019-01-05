import Data.Bits
import Data.List.Split

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let samples = map parseSample $ chunksOf 4 ls
    let cpu = CPU [3,2,1,1]
    putStrLn $ show $ part1 samples

type Op = [Int]
data Sample = Sample CPU Op CPU deriving (Show)

parseSample :: [String] -> Sample
parseSample sample = let
    digits :: [Int]
    digits = map read $ extractDigits $ concat sample
    cpu = CPU (take 4 digits)
    op = take 4 . drop 4 $ digits
    expected = CPU (take 4 . drop 8 $ digits)
    in Sample cpu op expected where
        extractDigits = filter (not . null) . splitOneOf "[ ] Before: A t,"

part1 :: [Sample] -> Int
part1 samples = length $ filter (\x -> length x > 2) $ map (filter (\x -> x == True) . testOps) samples where
    testOps (Sample cpu [_,a,b,c] expected) = map (\f -> (f cpu a b c) == expected) allOperations

-- part2 :: [Sample]
part2 samples = let
    candidates = map (\x -> (x, [0..15])) allOperations
    in undefined where
        part2' (x, [], _) = (x,[])
        part2' (resolved, unresolved, ((Sample _ [a,_,_,_] _):xs)) = undefined

-- CPU and operations impl.
type Register = Int
data CPU = CPU
    { reg :: [Register] 
    } deriving (Show, Eq)

getRegister :: CPU -> Int -> Int
getRegister (CPU [a,_,_,_]) 0 = a
getRegister (CPU [_,b,_,_]) 1 = b
getRegister (CPU [_,_,c,_]) 2 = c
getRegister (CPU [_,_,_,d]) 3 = d
getRegister _ _ = undefined

setRegister :: CPU -> Int -> Int -> CPU
setRegister (CPU [_,b,c,d]) 0 val = CPU [val,b,c,d]
setRegister (CPU [a,_,c,d]) 1 val = CPU [a,val,c,d]
setRegister (CPU [a,b,_,d]) 2 val = CPU [a,b,val,d]
setRegister (CPU [a,b,c,_]) 3 val = CPU [a,b,c,val]
setRegister _ _ _ = undefined

addr :: CPU -> Int -> Int -> Int -> CPU
addr cpu a b c = setRegister cpu c $ (getRegister cpu a) + (getRegister cpu b)

addi :: CPU -> Int -> Int -> Int -> CPU
addi cpu a b c = setRegister cpu c $ (getRegister cpu a) + b

mulr :: CPU -> Int -> Int -> Int -> CPU
mulr cpu a b c = setRegister cpu c $ (getRegister cpu a) * (getRegister cpu b)

muli :: CPU -> Int -> Int -> Int -> CPU
muli cpu a b c = setRegister cpu c $ (getRegister cpu a) * b

banr :: CPU -> Int -> Int -> Int -> CPU
banr cpu a b c = setRegister cpu c $ (getRegister cpu a) .&. (getRegister cpu b)

bani :: CPU -> Int -> Int -> Int -> CPU
bani cpu a b c = setRegister cpu c $ (getRegister cpu a) .&. b

borr :: CPU -> Int -> Int -> Int -> CPU
borr cpu a b c = setRegister cpu c $ (getRegister cpu a) .|. (getRegister cpu b)

bori :: CPU -> Int -> Int -> Int -> CPU
bori cpu a b c = setRegister cpu c $ (getRegister cpu a) .|. b

setr :: CPU -> Int -> Int -> Int -> CPU
setr cpu a _ c = setRegister cpu c $ getRegister cpu a

seti :: CPU -> Int -> Int -> Int -> CPU
seti cpu a _ c = setRegister cpu c a

gtir :: CPU -> Int -> Int -> Int -> CPU
gtir cpu a b c = if a > (getRegister cpu b) 
                 then setRegister cpu c 1 
                 else setRegister cpu c 0

gtri :: CPU -> Int -> Int -> Int -> CPU
gtri cpu a b c = if (getRegister cpu a) > b 
                 then setRegister cpu c 1 
                 else setRegister cpu c 0

gtrr :: CPU -> Int -> Int -> Int -> CPU
gtrr cpu a b c = if (getRegister cpu a) > (getRegister cpu b)
                 then setRegister cpu c 1 
                 else setRegister cpu c 0

eqir :: CPU -> Int -> Int -> Int -> CPU
eqir cpu a b c = if a == (getRegister cpu b)
                 then setRegister cpu c 1
                 else setRegister cpu c 0

eqri :: CPU -> Int -> Int -> Int -> CPU
eqri cpu a b c = if (getRegister cpu a) == b
                 then setRegister cpu c 1
                 else setRegister cpu c 0

eqrr :: CPU -> Int -> Int -> Int -> CPU
eqrr cpu a b c = if (getRegister cpu a) == (getRegister cpu b)
                 then setRegister cpu c 1
                 else setRegister cpu c 0

allOperations = [addr, addi, mulr, muli, banr, bani, borr, bori, setr, seti, gtir, gtri, gtrr, eqir, eqri, eqrr]