module Board where  -- do NOT CHANGE export of module

-- IMPORTS HERE
-- Note: Imports allowed that DO NOT REQUIRE TO ANY CHANGES TO package.yaml, e.g.:
--       import Data.Chars


-- #############################################################################
-- ############# GIVEN IMPLEMENTATION                           ################
-- ############# Note: "deriving Show" may be deleted if needed ################
-- #############       Given data types may NOT be changed      ################
-- #############################################################################

data Player = Red | Blue deriving Show
data Cell =  Stack [Player] | Empty deriving Show
data Pos = Pos { col :: Char, row :: Int } deriving Show
data Dir = North | NorthEast | East | SouthEast | South | SouthWest | West | NorthWest deriving (Show, Eq)
type Board = [[Cell]]

instance Eq Pos where
  (==) (Pos c1 r1) (Pos c2 r2) = (c1 == c2) && (r1 == r2)

instance Eq Player where
  (==) Blue Blue = True
  (==) Red Red = True
  (==) _ _ = False

instance Eq Cell where
  (==) Empty Empty = True
  (==) (Stack xs) (Stack ys) = xs == ys
  (==) _ _ = False


-- #############################################################################
-- ################# IMPLEMENT validateFEN :: String -> Bool ###################
-- ################## - 2 Functional Points                  ###################
-- ################## - 1 Coverage Point                     ###################
-- #############################################################################

-- Wann ist String gültig?
--1. wenn buchstaben nur r und b sind
--2. wenn 5 * / zu sehen ist und 30 * ,
--3. Evtl. Startaufstellung??




-- had to implement own split function because Data.List.Split import didn't work
-- Used ChatGPT to modify this split function, please see documentation for more information
splitFEN :: Char -> String -> [String]
splitFEN char "" = [""]
splitFEN char string = let (word, rest) = break (==char) string
                    in word : if null rest then [] else splitFEN char (tail rest)


validateR :: String -> Bool
validateR "" = False
validateR row = let fen = splitFEN ',' row
                in length fen == 6 && all validate fen
                where validate :: String -> Bool
                      validate = all (`elem` "rb")


validateFEN :: String -> Bool
validateFEN "" = False
validateFEN input =
    let rows = splitFEN '/' input
    in length rows == 6 && all validateR rows -- This line was partially modified using Copilot 



-- #############################################################################
-- ####################### buildBoard :: String -> Board #######################
-- ####################### - 2 Functional Points         #######################
-- ####################### - 1 Coverage Point            #######################
-- #############################################################################

-- This function (especially indentation) was partially modified using Copilot 
buildBoard :: String -> Board
buildBoard str = map rowRes (splitFEN '/' str)
                where rowRes :: String -> [Cell]
                      rowRes row = map checkField (splitFEN ',' row)
                        where checkField :: String -> Cell
                              checkField "" = Empty
                              checkField field = Stack $ map toPlayer field
                                  where toPlayer :: Char -> Player
                                        toPlayer 'r' = Red
                                        toPlayer 'b' = Blue
                              



-- #############################################################################
-- #################### path :: Pos -> Dir -> Int -> [Pos]  ####################
-- #################### - 4 Functional Points               ####################
-- #################### - 1 Coverage Point                  ####################
-- #############################################################################

path:: Pos -> Dir -> Int -> [Pos]
path stop _ 0 = [stop]
path start dir steps = start : path (nextPos dir start) (dirNew dir start) (steps - 1)

dirNew :: Dir -> Pos -> Dir
dirNew dir (Pos col row)
    | dir == South && row == 1  = North 
    | dir == North && row == 6  = South 
    | dir == East && col == 'f' = West  
    | dir == West && col == 'a' = East  
    | dir == NorthEast && row == 6 && col == 'f' = SouthWest 
    | dir == NorthWest && row == 6 && col == 'a' = SouthEast
    | dir == SouthEast && row == 1 && col == 'f' = NorthWest 
    | dir == SouthWest && row == 1 && col == 'a' = NorthEast 
    | dir == NorthEast && row < 6 && col == 'f' = NorthWest 
    | dir == NorthWest && row < 6 && col == 'a' = NorthEast 
    | dir == SouthEast && row > 1 && col == 'f' = SouthWest 
    | dir == SouthWest && row > 1 && col == 'a' = SouthEast 
    | dir == SouthWest && row == 1 && col > 'a' = NorthWest 
    | dir == SouthEast && row == 1 && col < 'f' = NorthEast 
    | dir == NorthWest && row == 6 && col > 'a' = SouthWest 
    | dir == NorthEast && row == 6 && col < 'f' = SouthEast 
    | otherwise = dir 


nextPos :: Dir -> Pos -> Pos
nextPos dir (Pos col row) =
    case dir of
        East      -> handleEast col row
        West      -> handleWest col row
        North     -> handleNorth col row
        South     -> handleSouth col row
        NorthEast -> handleNorthEast col row
        NorthWest -> handleNorthWest col row
        SouthEast -> handleSouthEast col row
        SouthWest -> handleSouthWest col row
  where
    handleEast c r --check
      | c == 'f' = Pos (pred c) r
      | otherwise = Pos (succ c) r

    handleWest c r --check
      | c == 'a' = Pos (succ c) r 
      | otherwise = Pos (pred c) r
    
    handleNorth c r --check
      | r == 6 = Pos c (r - 1)
      | otherwise = Pos c (r + 1)
    
    handleSouth c r -- check
      | r == 1 = Pos c (r + 1)
      | otherwise = Pos c (r - 1)
    
    handleNorthEast c r --check
      | c == 'f' && r == 6 = Pos (pred c) (r - 1)
      | c == 'f' && r < 6 = Pos (pred c) (r + 1)
      | c < 'f' && r == 6 = Pos (succ c) (r - 1)
      | otherwise          = Pos (succ c) (r + 1)

    handleNorthWest c r --check
      | c == 'a' && r == 6 = Pos (succ c) (r - 1) 
      | c == 'a' && r < 6 = Pos (succ c) (r + 1)
      | c > 'a' && r == 6 = Pos (pred c) (r - 1)
      | otherwise          = Pos (pred c) (r + 1)

    handleSouthEast c r -- check
      | c == 'f' && r == 1 = Pos (pred c) (r + 1)
      | c < 'f' && r == 1 = Pos (succ c) (r + 1)
      | c == 'f' && r > 1 = Pos (pred c) (r - 1)
      | otherwise          = Pos (succ c) (r - 1)

    handleSouthWest c r -- check 
      | c > 'a' && r == 1 = Pos (pred c) (r + 1)
      | c == 'a' && r == 1 = Pos (succ c) (r + 1)
      | c == 'a' && r > 1  = Pos (succ c) (r - 1)
      | otherwise          = Pos (pred c) (r - 1)
