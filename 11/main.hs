import Data.List
import Data.List.Split

main :: IO()
main = do
    -- putStrLn $ show $ grid
    mapM_ (putStrLn . show) $ (grid 5)
    putStrLn $ "      "
    putStrLn $ show $ part1 300
    putStrLn $ show $ part2

grid :: Int -> [[Int]]
grid size = chunksOf size $ map cellPower [(x,y) | x <- [1..size], y <- [1..size]] where

cellPower :: Integral a => (a, a) -> a
cellPower (x,y) = let
    rackId = x + 10
    serialNo = 7857
    largeNum = (rackId * y + serialNo) * rackId
    in (largeNum `div` 100 `mod` 10) - 5
  
sum3 :: Num a => [a] -> [a]
sum3 (x:y:[]) = []
sum3 (x:y:z:rest) = (x+y+z) : sum3 (y:z:rest)

part1 :: Int -> (Int, (Int, Int))
part1 gridSize = maximumBy (\(x, _) (y, _) -> compare x y) $ zip gr coords where
    powerSums = ((map sum3) . transpose . (map sum3))
    gr = concat $ powerSums (grid gridSize)
    coords = [(y,x) | x <- [1..gridSize-2], y <- [1..gridSize-2]]

sumX :: Num a => Int -> [a] -> [a]
sumX size xs
    | size > length xs = []
    | otherwise = sum (take size xs) : sumX size (tail xs)

part2 = maxBy $ map (maxBy . gen) [1..300] where
    gr = grid 300
    powerSums size = ((map (sumX size)) . transpose . (map (sumX size)))
    coords size = [(y,x,size) | x <- [1..(301-size)], y <- [1..(301-size)]]
    gen size = zip (concat ((powerSums size) gr)) (coords size)
    maxBy = maximumBy (\(x, _) (y, _) -> compare x y)