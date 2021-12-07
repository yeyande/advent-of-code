import Data.List
import Data.List.Split

parse :: String -> [Int]
parse s = map read $ splitOn "," s

getCrabMovement :: [Int] -> Int -> (Int, Int)
getCrabMovement positions destination = (destination, sum $ map (\x-> abs (x-destination)) positions)

solve :: String -> (Int, Int)
solve s = minimumBy (\(_, fuel) (_, fuel') -> compare fuel fuel') $ map (\x -> getCrabMovement sortedCrabs x) bounds
    where sortedCrabs = sort $ parse s
          bounds = [(minimum sortedCrabs)..(maximum sortedCrabs)]

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve input)
