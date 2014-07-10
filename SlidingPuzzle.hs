import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Maybe (fromJust, mapMaybe)
import Data.List (elemIndex)
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State.Lazy
import System.Environment (getArgs)

-- | Generalizing this solution for different dimensions of the board.
-- It can be 2x2, 3x3, 4x4 and so on.
type BoardDimension = Int

-- | Simple representation of the board using vector
type Board = Vector Int

-- | Deapth configuration for IDA* search
type DeapthLimit = Int

-- | Possible moves for the empty tile
data MoveDirection = North | East | South | West

-- | State of the puzzle during the search for solution. Keeps some information
-- regarding how we got to the current board (we can reconstruct the whole path
-- using this information)
data PuzzleState = PuzzleState { board :: Board	               -- current board representation
                               , dimension :: Int              -- board dimension (dimension x dimension)
                               , emptyTile :: Int              -- empty tile location
                               , distance :: Int               -- manhattan distance of the entire board
                               , moves :: Int                  -- number of moves it took us to get to this board
                               , previous :: Maybe PuzzleState -- previous state we came from
                               } deriving (Show, Eq, Ord)

-- | Convert matrix indicies to vector index
m2v :: Int -> Int -> Int -> Int
m2v n row column = n * row + column

-- | Convert vector index to matrix indicies
v2m :: Int -> Int -> (Int, Int)
v2m n index = (index `div` n, index `mod` n)

-- | Given the list of tiles generate our board representation
boardFromList :: [Int] -> Board
boardFromList = V.fromList

-- | Given CORRECT input (no validation) we get the board dimension and
-- the list of tiles from it.
parseInput :: String -> (BoardDimension, [Int])
parseInput input =
    let [n]:tiles = map (map read . words) (lines input)
    in (n, concat tiles)

-- | Sliding puzzle can be solved only if this condition is held:
-- *** zeroRow + numberOfInversions must be even ***
-- where zeroRow is the row number of empty tile (row index starts from 1)
-- numberOfInversions is the amount of elements Ai and Aj such that i < j but Ai > Aj
isSolvable :: BoardDimension -> [Int] -> Bool
isSolvable n tiles =
    let zeroRow = 1 + fromJust (0 `elemIndex` tiles) `div` n
        numberOfInversions = length [x | (x, xi) <- zip tiles [1..] :: [(Int,Int)], (y, yi) <- zip tiles [1..], x /= 0, y /= 0, yi > xi, x > y]
    in (zeroRow + numberOfInversions) `mod` 2 == 0

-- | Check if all the tiles are at the correct place
isSolutionFound :: PuzzleState -> Bool
isSolutionFound puzzleState = distance puzzleState == 0

-- | Update puzzle state after switching empty tile with tile found at position (row, column)
updatePuzzleState :: PuzzleState -> Int -> Int -> PuzzleState
updatePuzzleState puzzleState row column =
    puzzleState { board = board'
                , emptyTile = k
                , distance = boardDistance n board'
                , moves = moves puzzleState + 1
                , previous = Just puzzleState }
    where
        n = dimension puzzleState
        k = m2v n row column
        b = board puzzleState
        board' = b // [(emptyTile puzzleState, b ! k), (k, 0)]

-- | Update puzzle state if the empty tile is not moving off the board
makeMove :: PuzzleState -> MoveDirection -> Maybe PuzzleState
makeMove puzzleState direction =
    case direction of
        North -> if row <= 0        then Nothing else Just $ updatePuzzleState puzzleState (row - 1) column
        East  -> if column >= n - 1 then Nothing else Just $ updatePuzzleState puzzleState row (column + 1)
        South -> if row >= n - 1    then Nothing else Just $ updatePuzzleState puzzleState (row + 1) column
        West  -> if column <= 0     then Nothing else Just $ updatePuzzleState puzzleState row (column - 1)
    where
        n = dimension puzzleState
        (row, column) = v2m n (emptyTile puzzleState)

-- | Find all possible states which can be achieved by making a move in any direction on the current board
generatePossibleStates :: PuzzleState -> [PuzzleState]
generatePossibleStates puzzleState = mapMaybe (makeMove puzzleState) [North, East, South, West]

-- | Manhattan distance of a tile at vector index on a board with dimensions n x n
manhattan :: Int -> Int -> Int -> Int
manhattan tile n index = if tile == 0 then 0 else rowDistance + columnDistance
    where
        (row, column) = v2m n index
        rowDistance = abs (row - ((tile - 1) `div` n))
        columnDistance = abs (column - ((tile - 1) `mod` n))

-- | Manhattan distance of the entire board
boardDistance :: BoardDimension -> Board -> Int
boardDistance n currentBoard = sum $ map (\index -> manhattan (currentBoard ! index) n index) [0..n*n-1]

-- | Construct the path from initial board to the solution
solutionPath :: PuzzleState -> [Board]
solutionPath puzzleState = reverse $ boards puzzleState
    where
        boards currentState = case previous currentState of
            Nothing -> [board currentState]
            Just previousState -> board currentState : boards previousState

-- | Deapth first search for the solution with given deapth limit which we do not exceed
--
-- ReaderT is responsible for storing constant data like dimensions of the board and deapth limit.
-- StateT (S.Set Board) is responsible for storing visited states so that we do not visit them again.
-- StateT XXX is responsible for keeping search state, which helps during the search and also when
-- the solution is found it helps to easily trace how we reached that solution.
search :: ReaderT DeapthLimit (StateT (S.Set Board) (StateT PuzzleState Maybe)) ()
search = do
    deapthLimit <- ask
    visited <- lift get
    puzzleState <- (lift . lift) get
    unless (isSolutionFound puzzleState) $ do
        let validStates s = S.notMember (board s) visited && distance s + moves s <= deapthLimit
        let possibleStates = filter validStates (generatePossibleStates puzzleState)
        case possibleStates of
            [] -> mzero
            _ -> msum $ map (\possibleState -> do lift $ put (S.insert (board possibleState) visited); lift . lift $ put possibleState; search) possibleStates

-- | IDA* search.
--
-- Given an initial deapth limit we try to find a solution and we are not successful
-- then we increase the limit by some constant value and try again. We do it until
-- the solution is finally found.
findSolution :: PuzzleState -> DeapthLimit -> Maybe [Board]
findSolution puzzleState deapthLimit =
    let solution = execStateT (runStateT (runReaderT search deapthLimit) (S.singleton (board puzzleState))) puzzleState
    in case solution of
        Just finalState -> Just $ solutionPath finalState
        Nothing -> findSolution puzzleState (deapthLimit + 3)

-- | Solve the sliding puzzle.
-- Make sure that the given puzzle is solvable and if it is then start the IDA* search.
-- If the search is unsuccessful then increase the deapth limit and repeat the search.
-- Do it until the solution is finally found.
solveSlidingPuzzle :: String -> Maybe [Board]
solveSlidingPuzzle input =
    let (n, tiles) = parseInput input
        solvable = isSolvable n tiles
        initialBoard = boardFromList tiles
        emptyTileLocation = fromJust $ V.elemIndex 0 initialBoard
        puzzleState = PuzzleState initialBoard n emptyTileLocation (boardDistance n initialBoard) 0 Nothing
    in if solvable
       then findSolution puzzleState (distance puzzleState)
       else Nothing

main :: IO()
main = do
    puzzle <- liftM (!! 0) getArgs >>= readFile
    let solution = solveSlidingPuzzle puzzle
    case solution of
        Nothing -> print "Given puzzle cannot be solved"
        Just path -> print path
