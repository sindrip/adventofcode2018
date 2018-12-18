import Data.List.Split
import Data.List
import Data.Char

main :: IO()
main = do
    file <- readFile "input.txt"
    putStrLn $ show $ part1 file
    putStrLn $ show $ part2 file

part1 xs = part1' [] xs
    where part1' [] (x:xs) = part1' [x] xs
          part1' (x:xs) (y:ys)
            | (isUpper x) /= (isUpper y) && toUpper x == toUpper y = part1' xs ys
            | otherwise = part1' (y:x:xs) ys
          part1' fin [] = length fin

part2 xs = minimum $ map part1 (candidates xs)
    where filt [] = []
          filt (x:xs) = filter (\c -> toLower c /= toLower x) : filt xs
          candidates x = map (\f -> f x) (filt ['a'..'z']) 
