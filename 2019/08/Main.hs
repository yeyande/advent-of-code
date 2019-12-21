import Data.List.Split
import Control.Exception
import Data.List
type Layer = [Int]
type Image = [Layer]

testData = "123456789012"

layerWidth = 25
layerHeight = 6

solve :: String -> Int -> Int -> Int
solve input width height = (numberOfOccurrences '1' leastZeroes) * (numberOfOccurrences '2' leastZeroes)
    where layerLength = width * height
          layers = chunksOf layerLength input
          leastZeroes = snd $ minimumBy (\a b -> compare (fst a) (fst b)) $ map (\layer -> (numberOfOccurrences  '0' layer, layer)) layers

numberOfOccurrences :: Char -> String -> Int
numberOfOccurrences e xs = length $ findIndices (==e) xs

main = do
    input <- readFile "input.txt"
    let answer = solve ((lines input) !! 0) layerWidth layerHeight
    putStrLn $ show answer

testExample :: Int
testExample = assert (got == expected) got
    where got = solve testData 3 2
          expected = 1
