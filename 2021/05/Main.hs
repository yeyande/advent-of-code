import Data.List.Split
import Data.List
type Point = (Int, Int)
data LineSeg = LineSeg { getStart :: Point, getEnd :: Point } deriving (Show)

toInt :: String -> Int
toInt x = read x

isHorizontal :: LineSeg -> Bool
isHorizontal (LineSeg (x1, _) (x2, _)) = x1 == x2

isVertical :: LineSeg -> Bool
isVertical (LineSeg (_, y1) (_, y2)) = y1 == y2

getPointsOnSeg :: LineSeg -> [Point]
getPointsOnSeg (LineSeg (x1, y1) (x2, y2))
    | x1 == x2 = case compare y1 y2 of
        LT -> [(x1, y) | y <- [y1..y2]]
        GT -> [(x1, y) | y <- [y2..y1]]
        EQ -> [(x1, y1)]
    | y1 == y2 = case compare x1 x2 of
        LT -> [(x, y1) | x <- [x1..x2]]
        GT -> [(x, y1) | x <- [x2..x1]]
        EQ -> [(x1, y1)]
    | otherwise = undefined

makeLineSegment :: String -> LineSeg
makeLineSegment seg = LineSeg (x1, y1) (x2, y2)
    where [[x1, y1], [x2, y2]] = map (\x -> map toInt (splitOn "," x)) (splitOn " -> " seg)

parse :: [String] -> [LineSeg]
parse l = map makeLineSegment l

solve :: [String] -> Int
solve l = length $ filter (\x -> x >= 2) $ map length $ group $ sort $ concat $ map getPointsOnSeg wantedSegs
    where segs = parse l
          wantedSegs = filter (\x -> isVertical x || isHorizontal x) segs

main :: IO()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
