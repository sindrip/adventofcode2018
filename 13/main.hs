
main :: IO()
main = do
    ls <- lines <$> readFile "tinput.txt"
    putStrLn ""
    mapM_ (putStrLn . show) ls

type Point = (Int, Int)
data Direction = Left | Right | Up | Down deriving (Show)
data Cart = Cart Point Direction deriving (Show)

