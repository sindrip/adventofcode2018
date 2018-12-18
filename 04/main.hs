import Data.List.Split
import Data.List
import Data.Char

main :: IO()
main = do
    file <- readFile "tinput.txt"
    let ls = lines file
    let dat = sort $ map parseLine ls
    -- let nums = map read ls :: [Int]
    putStrLn $ show $ dat
    -- putStrLn $ show $ part1 dat
    -- putStrLn $ show $ part2 nums

data Timestamp = Timestamp
    { year :: Int
    , month :: Int
    , date :: Int
    , hour :: Int
    , minute :: Int
    } deriving (Eq, Ord, Show)

getMinute :: Timestamp -> Int
getMinute (Timestamp _ _ _ _ m) = m

data Event = Guard Int | FallsAsleep | WakesUp deriving (Show, Eq, Ord)

parseLine :: String -> (Timestamp, Event)
parseLine str = (parseTimestamp ts, parseEvent d)
    where [ts, d] = splitOn "]" str
          parseTimestamp = toTimestamp . map read . filter (not . null) . splitOneOf "[-: "
          toTimestamp [y, m, d, h, min] = Timestamp y m d h min
          parseEvent x
            | x == " wakes up" = WakesUp
            | x == " falls asleep" = FallsAsleep
            | otherwise = Guard (parseGuardId x)
          parseGuardId = read . (filter isDigit)

-- part1 :: [(Timestamp, Event)] -> Int
-- part1 ((x,e):xs) = case e of Guard i -> 
--     where (Timestamp _ _ _ _ m, event) = x
--           eat = [(getMinute . fst . head) xs .. (getMinute . fst . head . tail) xs]
--         --   next ys = dropWhile (coFallsAsleep)