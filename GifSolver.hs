import Data.List.Split (chunksOf)
import Diagrams.Prelude
import Diagrams.Backend.Rasterific.CmdLine

import SlidingPuzzle

draw :: Int -> Diagram B R2
draw v = text (label v)
           # fontSize (Normalized 0.12)
           # bold
          <> roundedRect 1 1 0.2
           # fc darkseagreen
           # lw thick
    where
        label 0 = ""
        label n = show n

boardDiagram :: Int -> [Int] -> Diagram B R2
boardDiagram n path = bg lightgray
                    . frame 0.1
                    . vcat' (with & sep .~ 0.05)
                    . map (hcat' (with & sep .~ 0.05) . map draw) $ rows
    where
        rows = chunksOf n path

diagrams :: Int -> [[Int]] -> [Diagram B R2]
diagrams n = map (boardDiagram n)

times :: Int -> [Int]
times n = replicate (n-1) 100 ++ [300]

gifs :: [[Int]] -> [(Diagram B R2, Int)]
gifs path = zip (diagrams n path) (times . length $ path)
    where
        n :: Int
        n = (floor . sqrt . fromIntegral) (length $ head path)

main :: IO()
main = do
    putStrLn "Enter the name of the file containing the puzzle specification: "
    puzzle <- getLine >>= readFile
    let solution = solveSlidingPuzzle puzzle
    case solution of
        Nothing -> print "Given puzzle cannot be solved"
        Just path -> mainWith $ gifs path
