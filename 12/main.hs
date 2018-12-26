import Data.List
import Data.List.Split
import qualified Data.Map.Strict as HM
import Data.Maybe

main :: IO()
main = do
    file <- readFile "input.txt"
    let ls = lines file
    let game = newGame ls
    putStrLn $ show $ part1 game
    -- putStrLn $ show $ part2 game
    putStrLn $ show $ part2
    -- mapM_ (putStrLn . show) rules

type GameState = [Char]
type Rules = (HM.Map [Char] Char)
data Game = Game Rules GameState Int deriving (Show)

parseRule :: String -> (String, Char)
parseRule input = (rule, (head plant)) where
    [rule, plant] = filter (not . null) . (splitOneOf " =>") $ input

newGame :: [String] -> Game
newGame ls = let
    state = drop 15 (head ls)
    ruleList = (tail . tail) ls
    rules = HM.fromList $ map parseRule ruleList
    in Game rules state 0

step :: Game -> Game
step (Game rules state firstPlantNo) = let
    pad = "....." ++ state ++ "....."
    newState = mapMaybe (\x -> HM.lookup x rules) (chunks pad) 
    in Game rules newState (firstPlantNo-3) where
        chunks :: String -> [String]
        chunks xs
            | length xs < 5 = []
            | otherwise = take 5 xs : chunks (tail xs)

plantSum :: Game -> Int
plantSum (Game _ state firstPlantNo) = sum . (map fst) $ filt $ zip [firstPlantNo..] state where
    filt = filter (\(x, y) -> y == '#')

part1 :: Game -> Int
part1 game = plantSum $ iterate step game !! 20

-- part2 game = take 500 $ (map plantSum) (iterate step game)
part2 = 39920 + 80 * (50000000000 - 499)

