import Data.Bits
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do
    ls <-lines <$> readFile "input.txt"
    let samples = map parseSample $ chunksOf 4 ls
    putStrLn $ show $ part1 samples
    putStrLn ""
    -- putStrLn $ show $ test samples
    putStrLn $ show $ findOpCode $ byOpcode samples !! 1
    putStrLn ""


data Sample = Sample
    { before :: CPU
    , instr :: [Int]
    , expected :: CPU
    , opNo :: Int
    } deriving (Show)

parseSample :: [String] -> Sample
parseSample sample = let
    digits :: [Int]
    digits = map read $ extractDigits $ concat sample
    cpu = newCPU (take 4 digits)
    instr = (take 3 . drop 5 $ digits)
    expected = newCPU (take 4 . drop 8 $ digits)
    opNo = head (drop 4 digits)
    in Sample cpu instr expected opNo where
        extractDigits = filter (not . null) . splitOneOf "[ ] Before: A t,"

part1 :: [Sample] -> Int
part1 samples = length . filter (\x -> length x > 2) $ map (filter (\x -> x == True) . testAll) samples where
    testAll (Sample cpu [a,b,c] expected _) = map (\x -> (step cpu (Instruction x a b c)) == expected ) [Addr ..]

testOps :: Sample -> [Op] -> [Op]
testOps (Sample cpu [a,b,c] expected opNo) ops = let
    instrs = map (\x -> Instruction x a b c) ops
    candidates = catMaybes $ map (\x -> if step cpu x == expected then Just x else Nothing) instrs
    in map (\(Instruction op _ _ _) -> op) candidates

byOpcode :: [Sample] -> [[Sample]]
byOpcode = (groupBy (comp (==)) . sortBy (comp (compare))) where
    comp f = (\(Sample _ _ _ x) (Sample _ _ _ y) -> f x y)

-- findOpCode :: [Sample] -> (Int, Op)
findOpCode samples = f' samples [Addr ..] where
    f' (sample:xs) ops
        -- | length ops == 1 = (opNo sample, head ops)
        | length ops == 1 = (length ops, head ops)
        | otherwise = f' xs (filter (\x -> x `elem` ops) $ testOps sample ops)
    f' [] ops = (length ops, Addr)

-- findOpCode samples = f' samples [Addr ..] where
--     f' (sample:xs) ops = f' xs (testOps sample ops)
--     f' [] ops = (length ops, Addr)

test samples = let
    cands = byOpcode samples
    in map findOpCode cands


data Op = Addr | Addi | Mulr | Muli
        | Banr | Bani | Borr | Bori
        | Setr | Seti | Gtir | Gtri
        | Gtrr | Eqir | Eqri | Eqrr
    deriving (Show, Eq, Enum)

data Instruction = Instruction Op Int Int Int deriving (Show)

data CPU = CPU
    { regA :: Int
    , regB :: Int         
    , regC :: Int
    , regD :: Int
    } deriving (Show, Eq)
newCPU :: [Int] -> CPU
newCPU [a,b,c,d] = CPU a b c d

getReg :: CPU -> Int -> Int
getReg (CPU a _ _ _) 0 = a
getReg (CPU _ b _ _) 1 = b
getReg (CPU _ _ c _) 2 = c
getReg (CPU _ _ _ d) 3 = d
getReg _ _ = undefined 

setReg :: CPU -> Int -> Int -> CPU
setReg (CPU _ b c d) 0 val = CPU val b c d
setReg (CPU a _ c d) 1 val = CPU a val c d
setReg (CPU a b _ d) 2 val = CPU a b val d
setReg (CPU a b c _) 3 val = CPU a b c val
setReg _ _ _ = undefined

step :: CPU -> Instruction -> CPU
step cpu (Instruction op a b c) = case op of
    Addr -> setReg cpu c $ (getReg cpu a) * (getReg cpu b)
    Addi -> setReg cpu c $ (getReg cpu a) + b
    Mulr -> setReg cpu c $ (getReg cpu a) * (getReg cpu b)
    Muli -> setReg cpu c $ (getReg cpu a) * b
    Banr -> setReg cpu c $ (getReg cpu a) .&. (getReg cpu b)
    Bani -> setReg cpu c $ (getReg cpu a) .&. b
    Borr -> setReg cpu c $ (getReg cpu a) .|. (getReg cpu b)
    Bori -> setReg cpu c $ (getReg cpu a) .|. b
    Setr -> setReg cpu c $ getReg cpu a
    Seti -> setReg cpu c a
    Gtir -> setReg cpu c $ fromEnum $ a > (getReg cpu b)
    Gtri -> setReg cpu c $ fromEnum $ (getReg cpu a) > b
    Gtrr -> setReg cpu c $ fromEnum $ (getReg cpu a) > (getReg cpu b)
    Eqir -> setReg cpu c $ fromEnum $ a == (getReg cpu b)
    Eqri -> setReg cpu c $ fromEnum $ (getReg cpu a) == b
    Eqrr -> setReg cpu c $ fromEnum $ (getReg cpu a) == (getReg cpu b)

