import Data.List
import Data.List.Split

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let points = map readPoint ls
    mapM_ (putStrLn . show) $ part1 points
    putStrLn $ show $ part2 points

--                 x   y   dx  dy
data Point = Point Int Int Int Int deriving (Show, Eq, Ord)

readPoint :: String -> Point
readPoint l = Point ix iy idx idy where
    [_,x,y,_,dx,dy] = filter (\x -> not $ null x) $ splitOneOf "<,> " l
    [ix, iy, idx, idy] = map read [x,y,dx,dy]

step :: [Point] -> [Point]
step = map update where
    update (Point x y dx dy) = Point (x+dx) (y+dy) dx dy

boundingBox :: [Point] -> (Int, Int, Int, Int)
boundingBox = foldr (\(Point x y _ _) (x0, x1, y0, y1) -> (min x x0, max x x1, min y y0, max y y1)) (maxb, minb, maxb, minb) where
    minb = minBound :: Int
    maxb = maxBound :: Int

dist2 :: [Point] -> Int
dist2 ps = (maxx - minx) + (maxy - miny) where
    (minx, maxx, miny, maxy) = boundingBox ps

part1 :: [Point] -> [String]
part1 ps = printPoints $ part1' (maxBound :: Int) ps where
    part1' dist points = if dist < nextDist then points else part1' nextDist nextStep where
        nextStep = step points
        nextDist = dist2 nextStep

printPoints :: [Point] -> [String]
printPoints ps = transpose $ map concat $ map (map draw) canvas where
    (minx, maxx, miny, maxy) = boundingBox ps
    groupByFst = (groupBy (\x y -> fst x == fst y))
    newPs = foldr (\(Point x y _ _) l -> ((x - minx), (y - miny)):l) [] ps
    canvas = groupByFst [(x,y) | x <- [0..(maxx-minx)], y <- [0..(maxy-miny)]]
    draw = (\x -> if x `elem` newPs then "#" else ".")

part2 :: [Point] -> Int
part2 ps = part2' (maxBound :: Int) 0 ps where
    part2' dist t points = if dist < nextDist then t else part2' nextDist (t+1) nextStep where
        nextStep = step points
        nextDist = dist2 nextStep