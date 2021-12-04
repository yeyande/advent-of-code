import Data.List.Split
import Data.Char
import Data.List 
import Numeric

data Rating = Rating {
            getIteration :: Int,
            values :: [[Int]]
        } deriving (Show)
parse :: [String] -> [[Int]]
parse l = map (\x -> map read $ chunksOf 1 x) l

mostCommonBit :: [[Int]] -> Int
mostCommonBit [zeroes, ones]
    | (length zeroes) > (length ones) = 0
    | otherwise = 1 
mostCommonBit [bit] = bit !! 0 

leastCommonBit :: Int -> Int
leastCommonBit 0 = 1
leastCommonBit 1 = 0

filterMostCommonVals :: Int -> [[Int]] -> [[Int]]
filterMostCommonVals iter bits = filter (\x -> (x !! iter) == (commonBits !! iter)) bits
    where commonBits = map (mostCommonBit . group . sort) $ transpose bits

step :: Rating -> Rating
step (Rating iter vals) = Rating iter2 newVals
    where iter2 = iter + 1
          newVals = filterMostCommonVals iter vals

filterLeastCommonVals :: Int -> [[Int]] -> [[Int]]
filterLeastCommonVals iter bits = filter (\x -> (x !! iter) == (commonBits !! iter)) bits
    where commonBits = map (leastCommonBit . mostCommonBit . group . sort) $ transpose bits

step2 :: Rating -> Rating
step2 (Rating iter vals) = Rating iter2 newVals
    where iter2 = iter + 1
          newVals = filterLeastCommonVals iter vals

solve :: [String] -> Int
solve lines = gammaRate * epsilonRate
    where bits = parse lines
          commonBits = concat $ map (show . mostCommonBit . group . sort) $ transpose bits
          uncommonBits = concat $ map (show . leastCommonBit . mostCommonBit . group . sort) $ transpose bits
          gammaRate = fst $ (readInt 2 (\x -> x `elem` "01") digitToInt $ commonBits) !! 0
          epsilonRate = fst $ (readInt 2 (\x -> x `elem` "01") digitToInt $ uncommonBits) !! 0

toBin :: String -> Int
toBin bitString = fst $ (readInt 2 (\x -> x `elem` "01") digitToInt $ bitString) !! 0

solve2 :: [String] -> Int
solve2 lines = oxygenRating * co2Rating
    where rating = Rating 0 (parse lines)
          oxygenRating = toBin $ concat $ map show $ head $ values $ step $ last $ takeWhile (\x -> (length $ values x) > 1) (iterate step rating)
          co2Rating = toBin $ concat $ map show $ head $ values $ step2 $ last $ takeWhile (\x -> (length $ values x) > 1) (iterate step2 rating)


main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
    putStrLn $ "Part 2: " ++ (show $ solve2 $ lines input)
