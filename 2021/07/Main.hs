import Data.List
import Data.List.Split

parse :: String -> [Int]
parse s = map read $ splitOn "," s

getFuelCost :: [Int] -> Int -> (Int, Int)
getFuelCost positions destination = (destination, sum $ map (\x-> abs (x-destination)) positions)

getFuelCost2 :: [Int] -> Int -> (Int, Int)
getFuelCost2 positions destination = (destination, sum $ map (\x-> sum $ map (\y -> abs (x-y)) [(minimum [x, destination])..(maximum [x, destination])]) positions)

solve :: String -> Int
solve s = snd $ minimumBy (\(_, fuel) (_, fuel') -> compare fuel fuel') $ map (\x -> getFuelCost sortedCrabs x) bounds
    where sortedCrabs = sort $ parse s
          bounds = [(minimum sortedCrabs)..(maximum sortedCrabs)]


solve2 :: String -> Int
solve2 s = snd $ minimumBy (\(_, fuel) (_, fuel') -> compare fuel fuel') $ map (\x -> getFuelCost2 sortedCrabs x) bounds
    where sortedCrabs = sort $ parse s
          bounds = [(minimum sortedCrabs)..(maximum sortedCrabs)]

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve input)
    putStrLn $ "Part 2: " ++ (show $ solve2 input)
