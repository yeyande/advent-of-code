getFuelToLaunch :: Int -> Int
getFuelToLaunch mass = (mass `div` 3) - 2

main :: IO ()
main = do
    input <- readFile "input.txt"
    let masses = map read $ lines input :: [Int]
    putStrLn . show . sum $ map getFuelToLaunch masses
