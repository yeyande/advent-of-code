import Data.List
data Parser = Parser { getCharacters :: [Char]
                     , getParsedCharacters :: [Char]
                     , getIllegalCharacters :: [Char]
                     } deriving (Show)

initParser :: [Char] -> Parser
initParser characters = Parser characters [] []

getClosingPair :: Char -> Char
getClosingPair '{' = '}'
getClosingPair '(' = ')'
getClosingPair '[' = ']'
getClosingPair '<' = '>'

getErrorScore :: Char -> Int
getErrorScore ')' = 3
getErrorScore ']' = 57
getErrorScore '}' = 1197
getErrorScore '>' = 25137


getAutocompleteScore :: Char -> Int
getAutocompleteScore ')' = 1
getAutocompleteScore ']' = 2
getAutocompleteScore '}' = 3
getAutocompleteScore '>' = 4

step :: Parser -> Parser
step (Parser (c:cs) p i )
    | c `elem` "([{<" = Parser cs (p ++ [c]) i
    | otherwise = case c == (getClosingPair (last p)) of
        True -> Parser cs (init p) i
        False -> Parser cs p (i ++ [c])

parse :: Parser -> Parser
parse p = step $ last $ takeWhile (\p -> (not $ null $ getCharacters p) && (null $ getIllegalCharacters p)) $ iterate step p

solve :: [String] -> Int
solve s = sum $ map (\p -> getErrorScore $ head $ getIllegalCharacters p) $ filter (\p -> not $ null $ getIllegalCharacters p) $ map parse parsers
    where parsers = map initParser s

solve2 :: [String] -> Int
solve2 s = scores !! ((length scores) `div` 2)
    where parsers = map initParser s
          incompleteParsers = filter (\p -> null $ getIllegalCharacters p) $ map parse parsers
          autocompleted = map (\p -> map getClosingPair (getParsedCharacters p)) incompleteParsers
          scores = sort $ map (\autocomplete -> foldr (\x acc -> (5*acc)+x) 0 $ map getAutocompleteScore autocomplete) autocompleted
          

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve $ lines input)
    putStrLn $ "Part 2: " ++ (show $ solve2 $ lines input)
