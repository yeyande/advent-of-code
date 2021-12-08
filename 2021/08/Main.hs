import Data.List.Split
parse :: [String] -> [([String], [String])]
parse inputLines = map (\line -> tupleOfWords $ splitOn " | " line)  inputLines
    where tupleOfWords [pattern, value] = (words pattern, words value)

solve :: [String] -> Int
solve input = length $ filter (\x -> x `elem` [2,3,4,7]) $ concat (map (\x -> map length $ snd x) parsed) 
    where parsed = parse input
main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
