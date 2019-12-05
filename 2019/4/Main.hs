import Data.List.Split
import Data.Char 
import Data.List

containsDouble :: String -> Bool
containsDouble [] = False
containsDouble [_] = False
containsDouble (a:xs)
    | a == (head xs) = True
    | otherwise = tailContainsDouble
    where tailContainsDouble = containsDouble xs

isNotDecreasing :: String -> Bool
isNotDecreasing [] = True
isNotDecreasing [_] = True
isNotDecreasing (a:xs)
    | a > (head xs) = False
    | otherwise = tailIsNotDecreasing
    where tailIsNotDecreasing = isNotDecreasing xs

isValidPassword :: String -> Bool
isValidPassword x = foldl (&&) True $ map (\f -> f x) [containsDouble, isNotDecreasing]

validPasswords :: [Int] -> [String]
validPasswords bounds = filter isValidPassword  [replicate (6 - (length $ show x)) '0' ++ show x | x <- bounds]

range :: String -> [Int]
range input = [start..stop]
        where [start, stop] = map read $ splitOn "-" input :: [Int]

solve :: String -> Int
solve input = length $ validPasswords $ range input

main = do
    input <- readFile "input.txt"
    let answer = solve $ input
    putStrLn $ show answer
