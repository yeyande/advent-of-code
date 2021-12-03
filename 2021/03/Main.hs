import Data.List.Split
import Data.Char
import Data.List 
import Numeric
parse :: [String] -> [[Int]]
parse l = map (\x -> map read $ chunksOf 1 x) l

mostCommonBit :: [[Int]] -> Int
mostCommonBit [zeroes, ones]
    | (length zeroes) > (length ones) = 0
    | otherwise = 1 

leastCommonBit :: Int -> Int
leastCommonBit 0 = 1
leastCommonBit 1 = 0

solve :: [String] -> Int
solve lines = gammaRate * epsilonRate
    where bits = parse lines
          commonBits = concat $ map (show . mostCommonBit . group . sort) $ transpose bits
          uncommonBits = concat $ map (show . leastCommonBit . mostCommonBit . group . sort) $ transpose bits
          gammaRate = fst $ (readInt 2 (\x -> x `elem` "01") digitToInt $ commonBits) !! 0
          epsilonRate = fst $ (readInt 2 (\x -> x `elem` "01") digitToInt $ uncommonBits) !! 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
