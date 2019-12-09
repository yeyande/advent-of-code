import Data.Graph
import Data.List.Split
import Data.List
import Control.Applicative
import Control.Exception

type GraphNode = (String, String, [String])

testData = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L"]

getVerticesOfInput :: [String] -> [String]
getVerticesOfInput = nub . concat . transpose . map (splitOn ")")
                    
edgesFromInput :: [String] -> [GraphNode]
edgesFromInput input = map (\(key,conns) -> (key, key, conns)) $ groupConnections $ map (splitOn ")") input

groupConnections :: [[String]] -> [(String, [String])]
groupConnections conns = connectedNodes ++ endpoints
                        where compareByNode a b = (head a) == (head b)
                              connectedNodes = map toNodeConnection $ groupBy compareByNode $ sort conns
                              endpoints = map (\e -> (e, [])) $ map head $ filter (\x -> not $ (head x) `elem` (map head conns)) $ map reverse conns

toNodeConnection :: [[String]] -> (String, [String])
toNodeConnection a = (head $ head a, concat $ map tail a)

parse :: [String] -> Graph
parse = (\(g, _, _)-> g) . graphFromEdges . edgesFromInput

maybeGetNode :: (Vertex -> GraphNode) -> (String -> Maybe Vertex) -> String -> Maybe GraphNode
maybeGetNode getNode getVertex query = liftA getNode $ getVertex query 

checksum :: Graph -> Int
checksum g = sum $ map (flip (-) 1) $ map length $ map (reachable g) $ vertices g 

solve :: [String] -> Int
solve = checksum . parse

testExample :: Int
testExample = assert (got == 42) got
            where got = solve testData

main = do
    input <- readFile "input.txt"
    let answer = solve $ lines input
    putStrLn $ show answer
