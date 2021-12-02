import qualified Data.List as L
import qualified Data.Maybe as M

data Motion = Motion { horizontal :: Int
                     , depth :: Int
                     , aim :: Int
    } deriving (Show)

initialMotion :: Motion
initialMotion = Motion 0 0 0

parse :: String -> Maybe Motion
parse x = 
    case L.elemIndex ' ' x of
        Just n -> case splitAt n x of
            ("forward", k) -> Just Motion{horizontal=read k, depth=0, aim=0}
            ("down", k)    -> Just Motion{horizontal=0, depth=read k, aim=read k}
            ("up", k)      -> Just Motion{horizontal=0, depth=negate (read k), aim=negate (read k)}
        Nothing -> Nothing

moveImmediate :: Motion -> Motion -> Motion
moveImmediate (Motion x1 depth1 _) (Motion x2 depth2 _) = Motion (x1+x2) (depth1+depth2) 0

moveWithAim :: Motion -> Motion -> Motion
moveWithAim (Motion horizontal depth aim) (Motion 0 _ aim2) = Motion horizontal depth (aim+aim2)
moveWithAim (Motion horizontal depth aim) (Motion distance _ _) = Motion (horizontal+distance) (depth+(aim*distance)) aim

solve :: (Motion -> Motion -> Motion) -> [String] -> Int
solve movementAlgorithm l = (horizontal motion)*(depth motion)
    where motion = foldl movementAlgorithm initialMotion (M.mapMaybe parse l)

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " ++ (show $ solve moveImmediate $ lines input)
    putStrLn $ "Part 2: " ++ (show $ solve moveWithAim $ lines input)
