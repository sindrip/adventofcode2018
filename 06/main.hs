-- import qualified Data.Set as Set
import Data.List
import Data.List.Split

main :: IO()
main = do
    file <- readFile "tinput.txt"
    let ls = lines file
    let points = readPoints ls
    let fPoints = finitePoints points
    -- let nums = map read ls :: [Int]
    putStrLn $ show $ points
    putStrLn $ show $ fPoints
    putStrLn $ show $ part1 points
    -- putStrLn $ show $ part2 nums

data Point = Point Int Int deriving (Show, Eq)

readPoints :: [String] -> [Point]
readPoints = map readPoint where
    readPoint ln = toPoint $ map read (splitOn "," ln)
    toPoint [x, y] = Point x y

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = abs x1 - x2 + abs y1 - y2

minMaxXs :: [Point] -> (Int, Int)
minMaxXs xs = (minimum ps, maximum ps) where 
    ps = foldr (\(Point x _) xs -> x : xs) [] xs

minMaxYs :: [Point] -> (Int, Int)
minMaxYs ys = (minimum ps, maximum ps) where
    ps = foldr (\(Point _ y) ys -> y : ys) [] ys

finitePoints :: [Point] -> [Point]
finitePoints ps = filter fp' ps where
    (x0, x1) = minMaxXs ps
    (y0, y1) = minMaxYs ps
    fp' (Point x y) = x > x0 && x < x1 && y > y0 && y < y1

-- part1 :: [Point] -> Int
part1 ps = grid where
    fPoints = finitePoints ps
    (x0, x1) = minMaxXs ps
    (y0, y1) = minMaxYs ps
    grid = [(Point x y) | x <- [x0..x1], y <- [y0..y1]]
    -- single ps = if length ps == 1 then Just (head ps) else Nothing
