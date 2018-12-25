-- import Data.List
import Data.List.PointedList.Circular
import qualified Data.Map.Strict as Map
import Data.Maybe

main :: IO()
main = do
    file <- readFile "input.txt"
    -- putStrLn $ show $ part1 ([],0,[]) 1
    putStrLn $ show $ part1 452 71250
    putStrLn $ show $ part1 452 7125000
    -- putStrLn $ show $ scoringTurns 9 500

-- scoringTurns :: Integral a => a -> a -> [[(a,a)]]
-- scoringTurns ps mt = playerTurns where
--     playerTurns = (sort . group) . (filter (\(x,y) -> y `mod` 23 == 0)) $ zip (cycle [1..ps]) [1..mt]

data Game = Game 
    { turn :: Int
    , list :: PointedList Int
    , scores :: Map.Map Int Int
    , maxPlayers :: Int
    } deriving (Show)

newGame :: Int -> Game
newGame mp = Game
    { turn = 1
    , list = PointedList [] 0 []
    , scores = Map.empty
    , maxPlayers = mp
    }

step :: Game -> Game
step (Game t l scores mp)
    | t `mod` 23 == 0 = Game newTurn (next $ fromJust $ deleteLeft scoring) newScores mp
    | otherwise = Game newTurn (insert t $ moveN 1 l) scores mp where
        newTurn = t+1
        scoring = moveN (-7) l
        toRemove (PointedList _ f _) = f
        sc = (t + (toRemove scoring))
        newScores = Map.insertWith (+) (t `mod` mp) sc scores

-- part1 :: Int -> Int -> Int
part1 playerCount lastWorth = let
    g = newGame playerCount
    endState = iterate step g !! lastWorth
    in foldr (\x y-> max x y) 0 $ sc endState
        where sc (Game _ _ scores _) = scores

    