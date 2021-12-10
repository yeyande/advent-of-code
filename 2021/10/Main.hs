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

getScore :: Char -> Int
getScore ')' = 3
getScore ']' = 57
getScore '}' = 1197
getScore '>' = 25137

step :: Parser -> Parser
step (Parser (c:cs) p i )
    | c `elem` "([{<" = Parser cs (p ++ [c]) i
    | otherwise = case c == (getClosingPair (last p)) of
        True -> Parser cs (init p) i
        False -> Parser cs p (i ++ [c])

parse :: Parser -> Parser
parse p = step $ last $ takeWhile (\p -> (not $ null $ getCharacters p) && (null $ getIllegalCharacters p)) $ iterate step p

solve :: [String] -> Int
solve s = sum $ map (\p -> getScore $ head $ getIllegalCharacters p) $ filter (\p -> not $ null $ getIllegalCharacters p) $ map parse parsers
    where parsers = map initParser s

main :: IO ()
main = undefined
