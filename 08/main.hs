import Data.List

main :: IO()
main = do
    file <- readFile "input.txt"
    let input = (map read) $ words file :: [Int]
    let tree = fst $ pt ([], input)
    putStrLn $ show $ part1 . head $ tree
    putStrLn $ show $ part2 . head $ tree

data Node = Node [Node] [Int] deriving (Show)

pt :: ([Node], [Int]) -> ([Node], [Int])
pt (n, (c:m:xs)) = (n ++ [Node nodes (take m rem)], drop m rem) where
    (nodes, rem) = iterate pt ([], xs) !! c

part1 :: Node -> Int
part1 (Node ns ms) = (sum ms) + sum (map part1 ns)

part2 :: Node -> Int
part2 (Node [] ms) = sum ms
part2 (Node ns ms) = foldr (\x y -> part2 (ns !! (x - 1)) + y) 0 $ safe where
    safe = filter (\x -> x <= length ns) ms



