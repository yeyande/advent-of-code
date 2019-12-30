import Data.List.Split
import Control.Exception
import Data.List
data Pixel = Transparent | Black | White deriving (Show)
type Layer = [[Pixel]]
type Image = [Layer]

testData = "0222112222120000"

layerWidth = 25
layerHeight = 6

solve :: String -> Int -> Int -> [String]
solve input width height = toDisplayCharacters $ merge layers
    where layerLength = width * height
          layers = parseLayers $ map (chunksOf width) $ chunksOf layerLength input
 
parseLayers :: [[String]] -> Image
parseLayers layers = map (map (map toPixel)) layers

toPixel :: Char -> Pixel
toPixel '0' = Black
toPixel '1' = White
toPixel '2' = Transparent

merge :: Image -> Layer
merge img = map (map (foldl1 resolvePixel)) $ map transpose $ transpose img

toDisplayCharacters :: Layer -> [String]
toDisplayCharacters layer = map (map pixelToCharacter) layer

pixelToCharacter :: Pixel -> Char
pixelToCharacter Black = '█'
pixelToCharacter White = ' '

resolvePixel :: Pixel -> Pixel -> Pixel
resolvePixel Transparent b = b
resolvePixel a _ = a

numberOfOccurrences :: Char -> String -> Int
numberOfOccurrences e xs = length $ findIndices (==e) xs

main = do
    input <- readFile "input.txt"
    let answer = solve ((lines input) !! 0) layerWidth layerHeight
    putStrLn $ unlines answer

testExample :: [String]
testExample = assert (got == expected) got
    where got = solve testData 2 2
          expected = ["█ ", " █"]
