getList :: String -> [Int]
getList s = map read $ words s

main :: IO()
main = do
    file <- readFile "input.txt"
    let x = sum $ getList file
    putStrLn $ show x