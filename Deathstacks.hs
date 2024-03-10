module Deathstacks where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO CHANGE package.yaml, e.g.:
import Board (splitFEN, Player (Red, Blue), Pos (Pos), Cell(Stack, Empty), Board, Dir(North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest), buildBoard, path)
import Data.Maybe (Maybe(Nothing))
import Data.Bool (Bool)



-- #############################################################################
-- ########################### GIVEN IMPLEMENTATION ############################
-- #############################################################################

data Move = Move {start :: Pos, target :: Pos, steps :: Int}

instance Show Move where
  show (Move (Pos startC startR) (Pos tarC tarR) tr) = [startC] ++ show startR ++ "-" ++ show tr ++ "-" ++ [tarC] ++ show tarR

instance Eq Move where
  (==) (Move (Pos sc1 sr1) (Pos tc1 tr1) r1) (Move (Pos sc2 sr2) (Pos tc2 tr2) r2) =
      sc1 == sc2 && sr1 == sr2 && tc1 == tc2 && tr1 == tr2 && r1 == r2


-- #############################################################################
-- #################### playerWon :: Board -> Maybe Player #####################
-- #################### - 4 Functional Points              #####################
-- #################### - 1 Coverage Point                 #####################
-- #############################################################################

-- Player wins if either player has control over all stacks on the board
-- board situation thats passed to method is always valid
playerWon :: Board -> Maybe Player
playerWon board
    | allStacks Red board = Just Red
    | allStacks Blue board = Just Blue
    | otherwise = Nothing

-- This function checks if a player has control over all stacks on the board
-- Function was partially modified using Copilot 
allStacks :: Player -> Board -> Bool
allStacks player [] = True
allStacks player (x:xs) = all controlledByPlayer x && allStacks player xs
  where controlledByPlayer (Stack (p:_)) = p == player
        controlledByPlayer Empty = True



-- #############################################################################
-- ################### possibleMoves :: Pos -> Cell -> [Move] ##################
-- ################### - 4 Functional Points                  ##################
-- ################### - 1 Coverage Point                     ##################
-- #############################################################################

-- Pseudocode:
-- check if Cell is not empty
-- step = length stack
-- while step != 0:
  -- do path for every direction with step and save as move
  -- check for dupes 
  -- step -= 1

-- This part of possibleMoves was modified using ChatGPT, please refer to ChatGPT Documentation
possibleMoves :: Pos -> Cell -> [Move]
possibleMoves pos (Stack stack) = removeDupes (concatMap (makeMoves pos (length stack)) [North, NorthEast, East, SouthEast, South, SouthWest, West, NorthWest]) -- This line was modified using Copilot
possibleMoves _ Empty = []

-- This function was partially modified using Copilot 
removeDupes :: [Move] -> [Move]
removeDupes [] = []
removeDupes (x:xs) = if x `elem` xs then removeDupes xs else x : removeDupes xs

makeMoves :: Pos -> Int -> Dir -> [Move]
makeMoves pos size dir = 
  filter (\(Move start end _) -> start /= end) $ -- this line was modified using ChatGPT, please refer to ChatGPT Documentation
    if size <= 4 
      then [Move pos (last (path pos dir steps)) steps | steps <- [1..size]] 
      else [Move pos (last (path pos dir steps)) steps | steps <- [(size-4)..size]]




-- #############################################################################
-- ################### isValidMove :: Board -> Move -> Bool ####################
-- ################### - 5 Functional Points                ####################
-- ################### - 1 Coverage Point                   ####################
-- #############################################################################

-- Pseudocode: 
-- get stack at pos 
-- check which player's turn it is
-- check other stacks from this player if they are stacks with more than 4 stones
  -- if yes, return false
-- check if move is in list of possible moves from this stack
  -- if yes, return true
  -- if no, return false

-- This function was modified using ChatGPT, please refer to ChatGPT Documentation
isValidMove :: Board -> Move -> Bool
isValidMove board move@(Move startPos targetPos steps) =
  let tooTall = findTooTall board
  in (null tooTall || (case lookup startPos tooTall of
          Just stack -> enoughSteps stack steps -- this line was modified using Copilot
          Nothing    -> False))

-- This function was modified using ChatGPT, please refer to ChatGPT Documentation
-- This function was also partially modified using Copilot
findTooTall :: Board -> [(Pos, Cell)]
findTooTall board = [(pos, cell) | (pos, cell) <- allCells board, isTooTall cell]

-- This function was modified using ChatGPT, please refer to ChatGPT Documentation
isTooTall :: Cell -> Bool
isTooTall (Stack stack) = length stack > 4
isTooTall _ = False

-- This function was modified using ChatGPT, please refer to ChatGPT Documentation
enoughSteps :: Cell -> Int -> Bool
enoughSteps (Stack stack) steps = length stack - steps <= 4

-- This function was modified using ChatGPT, please refer to ChatGPT Documentation
allCells :: Board -> [(Pos, Cell)]
allCells board = concatMap rowWithPos indexedBoard
  where
    indexedBoard = zip [1..6] board

    rowWithPos :: (Int, [Cell]) -> [(Pos, Cell)]
    rowWithPos (rowIndex, row) = zipWith cellWithPos ['a'..'f'] row --this line was modified using Copilot
      where
        cellWithPos :: Char -> Cell -> (Pos, Cell)
        cellWithPos colIndex cell = (Pos colIndex rowIndex, cell) -- This line was modified using Copilot


-- #############################################################################
-- ############# IMPLEMENT listMoves :: Board -> Player -> [Move] ##############
-- ############# - 2 Functional Points                            ##############
-- ############# - 1 Coverage Point                               ##############
-- #############################################################################

--Pseudocode:
-- get every stack and do the following:
    -- if first character != player:
        -- continue to next stack
      --else:
        -- get possible moves for this stack
        -- check if move isValidMove
        -- if yes, add to list of moves
        -- if no, continue to next move

-- Function was modified using ChatGPT, please refer to ChatGPT Documentation
-- Indentation of this function was modified using Copilot 
listMoves :: Board -> Player -> [Move]
listMoves board player = concatMap validMoves playerStack
  where
    playerStack :: [(Pos, Cell)]
    playerStack = filter (isPlayer player) (allCells board)

    isPlayer :: Player -> (Pos, Cell) -> Bool
    isPlayer player (_, Stack (p:_)) = p == player
    isPlayer _ _ = False

    validMoves :: (Pos, Cell) -> [Move]
    validMoves (pos, stack) = filter (isValidMove board) (possibleMoves pos stack)
