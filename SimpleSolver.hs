import Control.Monad (liftM)
import System.Environment (getArgs)

import SlidingPuzzle

main :: IO()
main = do
    puzzle <- liftM (!! 0) getArgs >>= readFile
    let solution = solveSlidingPuzzle puzzle
    case solution of
        Nothing -> print "Given puzzle cannot be solved"
        Just path -> do print path; putStrLn $ "Number of steps to solve: " ++ show (length path)
