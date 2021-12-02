import qualified Data.List as L
import qualified Data.Maybe as M

data Direction = Forward Int | Down Int | Up Int

parse :: String -> Maybe (Int, Int)
parse x = 
    case L.elemIndex ' ' x of
        Just n -> case splitAt n x of
            ("forward", k) -> Just (read k, 0)
            ("down", k)    -> Just (0, read k)
            ("up", k)      -> Just (0, negate (read k))
        Nothing -> Nothing


solve :: [String] -> Int
solve l = (fst coordinates) * (snd coordinates)
    where coordinates = foldl (\(x1, y1) (x2, y2) -> (x1+x2, y1+y2)) (0, 0) (M.mapMaybe parse l)

parse2 :: String -> Maybe (Int, Int, Int)
parse2 x = 
    case L.elemIndex ' ' x of
        Just n -> case splitAt n x of
            ("forward", k) -> Just (read k, 0, 0)
            ("down", k)    -> Just (0, 0, read k)
            ("up", k)      -> Just (0, 0, negate (read k))
        Nothing -> Nothing

solve2 :: [String] -> Int
solve2 l = x*y
    where (x, y, _) = foldl moveSubmarine (0,0,0) (M.mapMaybe parse2 l)

moveSubmarine :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
moveSubmarine (x, depth, aim) (0, 0, aim2) = (x, depth, aim+aim2)
moveSubmarine (x, depth, aim) (distance, 0, 0) = (x+distance, depth+(aim*distance), aim)

main :: IO ()
main = do
    input <- readFile "input.txt"
    let part1 = solve $ lines input
    let part2 = solve2 $ lines input
    putStrLn $ "Part 1: " ++ (show part1)
    putStrLn $ "Part 2: " ++ (show part2)
