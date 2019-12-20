import Data.Graph
import Data.Tree
import Data.List.Split
import Data.List
import Control.Applicative
import Control.Exception

type GraphNode = (String, String, [String])

testData = ["COM)B", "B)C", "C)D", "D)E", "E)F", "B)G", "G)H", "D)I", "E)J", "J)K", "K)L", "K)YOU", "I)SAN"]

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

parse :: [String] -> (Graph, (String -> Maybe Vertex))
parse = (\(g, _, gv)-> (g, gv)) . graphFromEdges . edgesFromInput

maybeGetNode :: (Vertex -> GraphNode) -> (String -> Maybe Vertex) -> String -> Maybe GraphNode
maybeGetNode getNode getVertex query = liftA getNode $ getVertex query 

checksum :: Graph -> Int
checksum g = sum $ map (flip (-) 1) $ map length $ map (reachable g) $ vertices g 

solve :: [String] -> Int
solve input = distance graph you santa
            where (graph, getVertex) = parse input
                  [you, santa] = map getVertex ["YOU", "SAN"]

distance :: Graph -> Maybe Vertex -> Maybe Vertex -> Int
-- You have to subtract 2 for the points of src and dst
distance g (Just src) (Just dst) = (flip (-) 2) $ sum $ map length [srcHops, dstHops]
    where [fromSrc, fromDst] = map (\p -> levels $ dfs g' [p] !! 0) [src, dst]
          g' = transposeG g
          common = intersect fromSrc fromDst
          [srcHops, dstHops] = map (filter (not . flip elem common)) [fromSrc, fromDst]

testExample :: Int
testExample = assert (got == 4) got
            where got = solve testData
                  

main = do
    input <- readFile "input.txt"
    let answer = solve $ lines input
    putStrLn $ show answer
