-- import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Maybe

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let points = readPoints ls
    putStrLn $ show $ part1 points
    putStrLn $ show $ part2 points

data Point = Point Int Int deriving (Show, Eq, Ord)

readPoints :: [String] -> [Point]
readPoints = map readPoint where
    readPoint ln = toPoint $ map read (splitOn "," ln)
    toPoint [x, y] = Point x y

dist :: Point -> Point -> Int
dist (Point x1 y1) (Point x2 y2) = abs (x1 - x2) + abs (y1 - y2)

boundingBox :: [Point] -> (Int, Int, Int, Int)
boundingBox ps = foldr (\(Point x y) (x0, x1, y0, y1) -> (min x x0, max x x1, min y y0, max y y1)) (maxInt, minInt, maxInt, minInt) ps where
    maxInt = maxBound :: Int
    minInt = minBound :: Int

grid :: (Int, Int, Int, Int) -> [Point]
grid (x0, x1, y0, y1) = [(Point x y) | x <- [x0..x1], y <- [y0..y1]]

gridEdge :: (Int, Int, Int, Int) -> [Point]
gridEdge (x0, x1, y0, y1) = filter pred $ grid (x0, x1, y0, y1) where
    pred (Point x y) = x == x0 || x == x1 || y == y0 || y == y1

-- Given a list of Points and a Point return the closest one, if tied return Nothing
closest :: [Point] -> Point -> Maybe Point
closest candidates p = closest' p candidates (0, [])  where
    closest' p (x:xs) (_, []) = closest' p xs (dist p x, [x])
    closest' _ [] (d, ys) = if length ys == 1 then Just (head ys) else Nothing
    closest' p (x:xs) (d, ys)
        | comp == LT = closest' p xs (dist p x, [x])
        | comp == EQ = closest' p xs (d, x:ys)
        | otherwise  = closest' p xs (d, ys)
            where comp = compare (dist p x) d

finitePoints :: [Point] -> [Point] -> [Point]
finitePoints ps edge = filter (\x -> (not . elem x) edgePoints) ps where
    edgePoints = catMaybes $ map (closest ps) edge

part1 :: [Point] -> Int
part1 points = maximum $ ((map length) . group . sort) $ filter (\x -> elem x fPoints) area where
    gEdge = (gridEdge . boundingBox) points
    g = (grid . boundingBox) points
    fPoints = finitePoints points gEdge
    area = catMaybes $ map (closest points) g

part2 :: [Point] -> Int
part2 points = length $ filter pred g where
    g = (grid . boundingBox) points
    pred p = (sum $ map (dist p) points) < 10000
