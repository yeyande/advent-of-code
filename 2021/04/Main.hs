import Data.List.Split
import Data.List

type BingoBoard = [[Int]]
data BingoGame = BingoGame { 
                            getNumbers :: [Int] 
                            , getBoards :: [BingoBoard]
                            , getToldNumbers :: [Int]
                            , hasWinner :: Bool
                            } deriving (Show)

parse :: [String] -> BingoGame
parse (numbers:boards) = BingoGame (map read $ splitOn "," numbers) (boards') [] False
    where boards' = chunksOf 5 $ map (\x -> map read x )$  map (filter (\x -> length x > 0 )) $ map (\x -> splitOn " " x) $ filter (\x -> length x > 0) boards

hasRun :: [Int] -> BingoBoard -> Bool
hasRun numbers board = any id $ map (all id) $ map (\x -> map (\y -> y `elem` numbers) x) (board ++ (transpose board))

step :: BingoGame -> BingoGame
step (BingoGame numbers boards toldNumbers _) = BingoGame numbers boards toldNumbers' hasWinner'
    where toldNumbers' = take (length toldNumbers+1) numbers
          hasWinner' = any id $ map (hasRun toldNumbers') boards

solve :: [String] -> Int
solve l = (last $ getToldNumbers winningGame) * (sum unmarkedNumbers)
    where bingo = parse l
          winningGame = step $ last $ takeWhile (not . hasWinner) $ iterate step bingo
          winningBoard = concat $ concat $ filter (hasRun (getToldNumbers winningGame)) (getBoards winningGame)
          unmarkedNumbers = filter (\x -> not $ x `elem` (getToldNumbers winningGame)) winningBoard

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
