import qualified Data.Map as M
import Data.List.Split
import Data.List

parse :: String -> M.Map Int Int
parse s = M.fromList $ map (\x -> (head x, length x)) $ group $ sort $ map read $ splitOn "," s

handleBirth :: Int -> Int
handleBirth (-1) = 6
handleBirth x = x

step :: M.Map Int Int -> M.Map Int Int
step fish = M.union (M.mapKeysWith (\x y -> x + y) handleBirth nextDay) newFish
    where nextDay = M.fromList $ map (\(k, v) -> (k-1, v)) $ M.toList fish
          newFish = case M.lookup (-1) nextDay of
            Just amount -> M.singleton 8 amount
            Nothing -> M.empty

solve :: Int -> String -> Int
solve days init = M.foldr (+) 0 $ last $ take (days+1) $ iterate step $ parse init 

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve 80 input)
    putStrLn $ "Part 2: " ++ (show $ solve 256 input)
