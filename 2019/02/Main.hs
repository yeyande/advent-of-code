import Data.List.Split
import Control.Exception

type Program = ([Int], Int, Bool)
examples = [
    ("1,9,10,3,2,3,11,0,99,30,40,50", ([3500,9,10,70,2,3,11,0,99,30,40,50],8,True)),
    ("1,0,0,0,99", ([2,0,0,0,99],4,True)),
    ("2,3,0,3,99", ([2,3,0,6,99],4,True)),
    ("2,4,4,5,99,0", ([2,4,4,5,99,9801],4,True)),
    ("1,1,1,4,99,5,6,0,99", ([30,1,1,4,2,5,6,0,99],8,True))
    ]

parseProgram :: String -> [Int]
parseProgram input = map read $ splitOn "," input :: [Int]

loadProgram :: String -> Program
loadProgram input = (parseProgram input, 0, False)

step :: Program -> Program
step prog
    | opcode == 99 = handleExitOpcode prog
    | opcode == 1  = handleArithmetic (+) prog
    | opcode == 2  = handleArithmetic (*) prog
    | otherwise    = error $ "Invalid opcode! " ++ show prog
    where opcode = app !! pc
          (app, pc, stop) = prog

handleExitOpcode :: Program -> Program
handleExitOpcode (app, pc, _) = (app, pc, True)

handleArithmetic :: (Int -> Int -> Int) -> Program -> Program
handleArithmetic operation app = updateProgramState appcode dst (operation a b) (programCounter+4) stop
                        where (appcode, programCounter, stop) = app
                              a   = lookupIndex appcode (programCounter + 1)
                              b   = lookupIndex appcode (programCounter + 2)
                              dst = appcode !! (programCounter + 3)

lookupIndex :: [Int] -> Int -> Int
lookupIndex app index = app !! (app !! index)

updateProgramState :: [Int] -> Int -> Int -> Int -> Bool -> Program
updateProgramState app dst src pc stop = (newApp, pc, stop)
                                       where newApp         = left ++ src : right
                                             (left,_:right) = splitAt dst app

end :: Program -> Bool
end (_,_,stop) = stop

run :: Program -> Program
run = until end step

replace :: Int -> Int -> Program -> Program
replace idx val (app, pc, stop) = updateProgramState app idx val pc stop

hasValidInitialValue :: Program -> Int -> Bool
hasValidInitialValue (app, _, _) v = (app !! 0) == v

main :: IO ()
main = do
    input <- readFile "input.txt"
    let prog = loadProgram input
    let combinations = map (\(x, y) -> (replace 1 x $ replace 2 y $ prog, (x,y))) [(x,y) | x <- [0..99], y <- [0..99]]
    let answer = (\(x,y) -> 100*x+y) $ snd $ head $ filter (\(app, iv) -> hasValidInitialValue (run app) 19690720) combinations
    putStrLn $ show answer

testExamples :: [Program]
testExamples =
    map shouldBeEqual (zip (map run $ map loadProgram startStates) endStates)
    where shouldBeEqual = \(got, expected) -> assert (got == expected) got
          (startStates, endStates) = unzip examples
