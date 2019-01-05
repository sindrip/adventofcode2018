import qualified Data.Set as Set

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let nums = map read ls :: [Int]
    putStrLn $ show $ part1 nums
    putStrLn $ show $ part2 nums

part1 :: Num a => [a] -> a
part1 xs = sum xs

part2 :: (Num a, Ord a) => [a] -> a
part2 xs = part2' Set.empty freqs
    where freqs = scanl (+) 0 (cycle xs)
          part2' s (x:xs) = if x `Set.member` s 
                              then x 
                              else part2' (x `Set.insert` s) xs
