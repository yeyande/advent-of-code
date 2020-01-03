import Control.Exception
import Data.List
testData = [
    ([".#..#", 
      ".....", 
      "#####", 
      "....#", 
      "...##"], 8),
    (["......#.#.",
      "#..#.#....",
      "..#######.",
      ".#.#.###..",
      ".#..#.....",
      "..#....#.#",
      "#..#....#.",
      ".##.#..###",
      "##...#..#.",
      ".#....####"], 33),
    (["#.#...#.#.",
      ".###....#.",
      ".#....#...",
      "##.#.#.#.#",
      "....#.#.#.",
      ".##..###.#",
      "..#...##..",
      "..##....##",
      "......#...",
      ".####.###."], 35),
    ([".#..#..###",
      "####.###.#",
      "....###.#.",
      "..###.##.#",
      "##.##.#.#.",
      "....###..#",
      "..#.#..#.#",
      "#..#.#.###",
      ".##...##.#",
      ".....#.#.."], 41),
    ([".#..##.###...#######",
      "##.############..##.",
      ".#.######.########.#",
      ".###.#######.####.#.",
      "#####.##.#.##.###.##",
      "..#####..#.#########",
      "####################",
      "#.####....###.#.#.##",
      "##.#################",
      "#####.##.###..####..",
      "..######..##.#######",
      "####.##.####...##..#",
      ".#####..#.######.###",
      "##...#.##########...",
      "#.##########.#######",
      ".####.#.###.###.#.##",
      "....##.##.###..#####",
      ".#.#.###########.###",
      "#.#.#.#####.####.###",
      "###.##.####.##.#..##"], 210)]

solve :: [String] -> Int
solve input = subtract 1 $ maximum $ map length $ map (nubBy isOverlap) vectors
    where vectors = map (\p -> map (vector p) asteroids) asteroids
          asteroids = asteroidCoordinates input

asteroidCoordinates :: (Enum a, Num a) => [String] -> [(a, a)]
asteroidCoordinates = concat . 
                      map indexToPairs . 
                      zip [0..] . 
                      map (map fromIntegral) . 
                      map (elemIndices '#')
    where indexToPairs = \(y, xs) -> map (\x -> (x, y)) xs

withSign :: (Show a, Num a) => a -> (Char, a)
withSign a = if '-' `elem` (show a) then ('-', a) else ('+', a)

slope :: Fractional a => (a, a) -> (a, a) -> a
slope (x1, y1) (x2, y2) = (y2-y1)/(x2-x1)

vector :: Num a => (a, a) -> (a, a) -> (a, a)
vector (x1, y1)  (x2, y2) = (x2-x1, y2-y1)

isOverlap :: (RealFloat a, Fractional a, Num a, Ord a) => (a, a) -> (a, a) -> Bool
isOverlap a b = and [slopesAreEqual (0,0) a b, quadrantsAreEqual a b]

slopesAreEqual :: (RealFloat a, Fractional a, Num a, Ord a) => (a, a) -> (a, a) -> (a, a) -> Bool
slopesAreEqual o a b = slopeA == slopeB && (isNegativeZero slopeA == isNegativeZero slopeB)
    where slopeA = slope o a
          slopeB = slope o b

quadrantsAreEqual :: (Fractional a, Num a, Ord a) => (a, a) -> (a, a) -> Bool
quadrantsAreEqual a b = quadrant a == quadrant b
          

quadrant :: (Fractional a, Num a, Ord a) => (a, a) -> Maybe Int
quadrant (x, y)
    | x > 0 && y > 0 = Just 1
    | x < 0 && y > 0 = Just 2
    | x < 0 && y < 0 = Just 3
    | x > 0 && y < 0 = Just 4
    | x == 0 || y == 0 = Nothing

main = do
    input <- readFile "input.txt"
    let answer = solve $ lines input
    putStrLn $ show answer

testExample :: [Int]
testExample = map test testData
    where test (input, expected) = assert ((solve input) == expected) expected
