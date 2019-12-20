import Data.List.Split
import qualified Data.List as List
import qualified Data.Set as Set
import Control.Exception
data Movement = Horizontal Int | Vertical Int deriving (Show)
type Point = (Int, Int)
type LineSegment = (Point, Point)

testData = [["R8,U5,L5,D3", "U7,R6,D4,L4"],
            ["R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"],
            ["R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"]
            ]
expectedTestOut = [6, 159, 135]

parse :: String -> [Movement]
parse input = List.map toMovement $ splitOn "," input

toMovement :: String -> Movement
toMovement movement
    | direction == 'L' = Horizontal $ negate distance
    | direction == 'U' = Vertical $ distance
    | direction == 'R' = Horizontal $ distance
    | direction == 'D' = Vertical $ negate distance
    where distance = read $ tail movement :: Int
          direction = head movement

toLineSegment :: Point -> Point -> LineSegment
toLineSegment start stop = (start, stop)

newLineSegment :: LineSegment -> Point -> LineSegment
newLineSegment (_, start) stop = (start, stop)

pointsOnLine :: LineSegment -> [Point]
pointsOnLine ((x1,y1), (x2,y2))
    | (x2 - x1) == 0 = [(x1, y) | y <- [starty..stopy]]
    | (y2 - y1) == 0 = [(x, y1) | x <- [startx..stopx]]
    where (starty, stopy) = (min y1 y2, max y1 y2)
          (startx, stopx) = (min x1 x2, max x1 x2)


toPoint :: Point -> Movement -> Point
toPoint (x, y) (Horizontal d) = (x+d, y)
toPoint (x, y) (Vertical d) = (x, y+d)

pointsAlongMovement :: String -> [Point]
pointsAlongMovement input = tail $ concat $ List.map pointsOnLine $ tail $ scanl newLineSegment ((0,0), (0,0)) $ tail $ scanl toPoint (0,0) $ parse input

compareManhattanDistance :: Point -> Point -> Ordering
compareManhattanDistance a b = compare (manhattanDistance a) (manhattanDistance b)

manhattanDistance :: Point -> Int
manhattanDistance (x, y) = (abs x) + (abs y)

solve :: [String] -> Int
solve input = manhattanDistance $ head $ List.sortBy compareManhattanDistance $ Set.toList $ Set.intersection w1 w2
    where [w1, w2] = List.map Set.fromList $ List.map pointsAlongMovement input

main = do
    input <- readFile "input.txt"
    let answer = solve $ lines input
    putStrLn $ show answer

testExamples :: [Int]
testExamples =
    List.map shouldBeEqual (zip (List.map solve testData) expectedTestOut)
    where shouldBeEqual = \(got, expected) -> assert (got == expected) got
