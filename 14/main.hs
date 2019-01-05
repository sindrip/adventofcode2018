import qualified Data.Sequence as Seq
import Data.Foldable

main :: IO()
main = do
    let game = newGame
    putStrLn $ show $ part1 game 209231
    -- putStrLn $ show $ part2 game 209231

type Elf = Int
type Recipes = Seq.Seq Int
data Game = Game 
    { elf1 :: Elf
    , elf2 :: Elf
    , recipes :: Recipes
    } deriving (Show)

newGame :: Game
newGame = Game 0 1 (Seq.fromList [3,7])

step :: Game -> Game
step (Game e1 e2 recipes) = let
    e1Recipe = (recipes `Seq.index` e1)
    e2Recipe = (recipes `Seq.index` e2)
    newCombined = (Seq.><) recipes $ (Seq.fromList . reverse . toAdd) $ e1Recipe + e2Recipe
    [newE1, newE2] = mod <$> [e1 + e1Recipe + 1, e2 + e2Recipe +1] <*> [length newCombined]
    in Game newE1 newE2 newCombined where
        toAdd :: Int -> [Int]
        toAdd x
            | x < 10 = [x]
            | otherwise = x `mod` 10 : toAdd (x `div` 10)

part1 :: Game -> Int -> Recipes
part1 game input = let
    ans = (recipes . head) $ filter (\x -> length (recipes x) >= (input+10)) (iterate step game)
    in Seq.take 10 . Seq.drop input $ ans

-- -- part2 :: Game -> Int -> Recipes
-- part2 game input = let
--     inputSeq = Seq.fromList $ getDigits input
--     last6 xs = (Seq.drop (Seq.length xs - 6)) xs
--     ans = (recipes . head) $ filter (\(Game _ _ r) -> last6 r == inputSeq) (iterate step game)
--     in (length ans) - 6 where
--         getDigits 0 = []
--         getDigits x = getDigits (x `div` 10) ++ [x `mod` 10]
