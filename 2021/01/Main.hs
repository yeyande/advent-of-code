depthHasIncreased :: (Ord a) => (a, a) -> Bool
depthHasIncreased (a, b)
    | a < b = True
    | otherwise = False

getWindow :: (Num a) => [a] -> [a]
getWindow xs =
    map (\(a,b,c) -> a+b+c) windows
    where windows = zip3 xs (drop 1 xs) (drop 2 xs)

getPairs :: [a] -> [(a, a)]
getPairs xs = zip xs (drop 1 xs)

solve :: (Ord a) => [(a, a)] -> Int
solve depthPairs = length $ filter (\x -> x) $ map depthHasIncreased depthPairs

main :: IO ()
main = do
    input <- readFile "input.txt"
    let depths = map read $ lines input :: [Int]
    let depthPairs = getPairs depths
    let depthWindowPairs = getPairs $ getWindow depths
    
    putStrLn ("Part 1: " ++ (show $ solve depthPairs))
    putStrLn ("Part 2: " ++ (show $ solve depthWindowPairs))

