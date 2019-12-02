getFuelToLaunch :: Int -> Int
getFuelToLaunch mass 
    | fuelMass > 0 = sum [fuelMass, getFuelToLaunch fuelMass]
    | otherwise    = 0
    where fuelMass = (mass `div` 3) - 2

main :: IO ()
main = do
    input <- readFile "input.txt"
    let masses = map read $ lines input :: [Int]
    putStrLn . show . sum $ map getFuelToLaunch masses
