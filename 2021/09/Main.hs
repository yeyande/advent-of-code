import qualified Data.Map as M
import Data.Maybe
import Data.Char (digitToInt)

type Point = (Int, Int)

parse :: [String] -> M.Map Point Int
parse l = M.fromList $ concat $ map (\(x, l) -> map (\(y, c) -> ((x,y), digitToInt c)) (zip [0..] l)) ((zip [0..] l))

isLocalMinimum :: M.Map Point Int -> Point -> Bool
isLocalMinimum m (x, y) = all (\x -> x > height) $ mapMaybe (\p -> M.lookup p m) pts
    where height = m M.! (x,y)
          pts = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

solve :: [String] -> Int
solve l = sum $ map (\pt -> (parsed M.! pt)+1) $ filter (\pt -> isLocalMinimum parsed pt) (M.keys parsed)
    where parsed = parse l

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
