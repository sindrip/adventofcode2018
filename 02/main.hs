import Data.List

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    -- let nums = map read ls :: [Int]
    putStrLn $ show $ part1 ls
    putStrLn $ show $ part2 ls

part1 :: Ord a => [[a]] -> Int
part1 xs = (length $ f2 c) * (length $ f3 c)
    where t = map (\x -> length x) . (group . sort)
          c = map t xs
          f2 xs = (filter . any) (==2) xs
          f3 xs = (filter . any) (==3) xs

part2 :: Eq a => [[a]] -> [a]
part2 (x:xs) = case part2' x xs of Nothing -> part2 xs
                                   Just y -> y
    where part2' _ [] = Nothing
          part2' x (y:ys) = if length c == 25 
                              then Just c 
                              else part2' x ys
            where c = comp x y
          comp [] [] = []
          comp (x:xs) (y:ys) = if x == y then x : r else r
            where r = (comp xs ys)

