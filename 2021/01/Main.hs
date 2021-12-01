depthHasIncreased :: (Int, Int) -> Bool
depthHasIncreased (a, b)
    | a < b = True
    | otherwise = False

main :: IO ()
main = do
    input <- readFile "input.txt"
    let depths = map read $ lines input :: [Int]
    let depthPairs = zip depths (drop 1 depths)
    
    putStrLn . show . length $ filter (\x -> x == True) $ map depthHasIncreased depthPairs
