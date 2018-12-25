import Data.Graph
import Data.List
import Data.Char
-- import qualified Data.Set as Set

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let edges = map parseEdge ls
    -- putStrLn $ show $ edges
    -- putStrLn $ show $ findFree edges
    putStrLn $ show $ part1 edges
    putStrLn $ show $ part2 edges

parseEdge :: String -> (Char, Char)
parseEdge ln = (head f, head t) where
    [_, f, _, _, _, _, _ , t, _, _] = words ln

findFree :: [(Char, Char)] -> [Char]
findFree xs = free where
    (s1, s2) = foldr (\(x0, x1) (y0, y1) -> (x0:y0, x1:y1)) ([],[]) xs
    u = (map head) . (group . sort)
    free = filter (\x -> not $ elem x (u s2)) (u s1)

part1 :: [(Char, Char)] -> [Char]
-- part1 [] = []
part1 edges
    | length edges == 1 = lastToList edges
    | otherwise = f : part1 newEdges where
        (f:fs) = findFree edges
        newEdges = filter (\(x, y) -> not (x == f || y == f)) edges
        lastToList [(x,y)] = [x,y]

charToSecond :: Char -> Int
charToSecond c = (ord c - 64) + 60

-- todo part2
