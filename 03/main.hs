import Data.List.Split
import Data.List
import Data.Char
import Data.Set as Set (Set, fromList, disjoint)

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let claims = map lineToClaim ls
    let sets = map claimSet claims
    -- let nums = map read ls :: [Int]
    putStrLn $ show $ part1 claims
    putStrLn $ show $ part2 claims sets

data Claim = Claim
    { claimId :: Int
    , x :: Int
    , y :: Int
    , w :: Int
    , h :: Int
    } deriving (Show)

lineToClaim :: String -> Claim
lineToClaim = toClaim . map read . filter (not . null) . splitOneOf "#@,:x "
    where
        toClaim [claimId, x, y, width, height] = Claim{ claimId = claimId, x = x, y = y, w = width, h = height}

claimPoints :: Claim -> [(Int, Int)]
claimPoints (Claim i x y w h) = [(x,y) | x <- [x..x+w-1], y <- [y..y+h-1]]

part1 :: Foldable t => t Claim -> Int
part1 claims = length $ s points
    where points = concatMap claimPoints claims
          s = filter (\x -> (length x) > 1) . (group . sort)

claimSet :: Claim -> Set (Int, Int)
claimSet c = fromList $ claimPoints c

part2 (x:xs) sets = if (part2' (claimSet x) sets) == 1 then x else part2 xs sets
    where part2' c [] = 0
          part2' c (x:xs) = if disjoint c x
                              then part2' c xs
                              else 1 + part2' c xs
                              