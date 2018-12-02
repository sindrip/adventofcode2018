import qualified Data.Set as Set

dup :: Ord a => [a] -> Maybe a
dup xs = dup' xs Set.empty
    where dup' [] _     = Nothing
          dup' (x:xs) s = if Set.member x s
                             then Just x
                             else dup' xs (Set.insert x s)

dupString :: (Ord a, Show a) => [a] -> String
dupString x = case dup x of
    Just x -> show x
    Nothing -> "No duplicate found"

getList :: String -> [Int]
getList s = Prelude.map read $ words s

timeStep :: [Int] -> [Int]
timeStep x = scanl (+) 0 c
    where c = cycle x

main :: IO()
main = do
    file <- readFile "input.txt"
    let x = timeStep $ getList file
    putStrLn $ dupString $ cycle x