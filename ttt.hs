-- TicTacToe
-- Haskell Practice
-- Sean Joseph

import Data.Char

data Space = E | X | O deriving(Eq)

instance Show Space where
  show X = "X"
  show O = "O"
  show E = " "

type Row = (Space,Space,Space)
type Board = (Row,Row,Row)

new_board :: Board
new_board = ((E,E,E),(E,E,E),(E,E,E))

row_winner :: Row -> Space
row_winner (a,b,c) | a == b && a == c = a
                   | otherwise        = E

decompose_board :: Board -> [Row]
decompose_board board = xs where
  (r1,r2,r3) = board
  (c1,c2,c3) = ((get_col 1 board),(get_col 2 board),(get_col 3 board))
  (fo,ba) = get_diags board
  xs = [r1,r2,r3,c1,c2,c3,fo,ba]

has_empty :: Row -> Bool
has_empty (c1,c2,c3) = c1 == E || c2 == E || c3 == E

moves_left :: Board -> Bool
moves_left (r1,r2,r3) = r1_space || r2_space || r3_space where
  r1_space = has_empty r1 
  r2_space = has_empty r2 
  r3_space = has_empty r3 

is_winner :: Board -> (Bool,Space)
is_winner board = (win,winner) where
  all_rows = decompose_board board
  winners = filter (/=E) (map row_winner all_rows)
  win = length winners > 0
  winner | win = head winners
         | otherwise = E

prev :: Space -> Space
prev X = O
prev O = X

next :: Space -> Space
next X = O
next O = X

move :: Int -> Int -> Space -> Board -> Board
move 1 col space (r1,r2,r3) = ((move_row col space r1),r2,r3)
move 2 col space (r1,r2,r3) = (r1,(move_row col space r2),r3)
move 3 col space (r1,r2,r3) = (r1,r2,(move_row col space r3))

move_row :: Int -> Space -> Row -> Row
move_row 1 space (_,c2,c3) = (space,c2,c3)
move_row 2 space (c1,_,c3) = (c1,space,c3)
move_row 3 space (c1,c2,_) = (c1,c2,space)

get_space :: Int -> Int -> Board -> Space
get_space row col = (get_elem col) . (get_row row)

get_row :: Int -> Board -> Row
get_row 1 (r1,_,_) = r1
get_row 2 (_,r2,_) = r2
get_row 3 (_,_,r3) = r3

get_elem :: Int -> Row -> Space
get_elem 1 (c1,_,_) = c1
get_elem 2 (_,c2,_) = c2
get_elem 3 (_,_,c3) = c3

get_col :: Int -> Board -> Row
get_col n board = ((get_space 1 n board),
                   (get_space 2 n board),
                   (get_space 3 n board))

get_diags :: Board -> (Row,Row)
get_diags board = (forward,back) where
  back    = ((get_space 1 1 board),
             (get_space 2 2 board),
             (get_space 3 3 board))
  forward = ((get_space 3 1 board),
             (get_space 2 2 board),
             (get_space 1 3 board))

valid_move :: Int -> Int -> Board -> Bool
valid_move row col board = good_row && good_col && empty where
  good_row = row > 0 && row <= 3
  good_col = col > 0 && col <= 3
  empty = E == (get_space row col board)
  
getDigit :: String -> IO Int
getDigit prompt = do
  putStr prompt
  x <- getChar
  newline
  if isDigit x then
    return (digitToInt x)
  else do
    putStrLn "ERROR: Invalid digit"
    getDigit prompt

putBoard :: Board -> IO ()
putBoard (r1,r2,r3) = do
  putStrLn "  C1  C2  C3 "
  putStr "R1"
  putRow r1
  putStr "  "
  putSep
  putStr "R2"
  putRow r2
  putStr "  "
  putSep
  putStr "R3"
  putRow r3

putSep :: IO ()
putSep = putStrLn "-----------"

putRow :: Row -> IO ()
putRow (c1,c2,c3) = do
  putStr " "
  putStr (show c1)
  putStr " | "
  putStr (show c2)
  putStr " | "
  putStr (show c3)
  putStrLn " "

newline :: IO ()
newline = putStrLn ""

ttt_game :: IO ()
ttt_game = ttt X (new_board)

ttt :: Space -> Board -> IO ()
ttt player board = do
  (win,winner) <- return (is_winner board)
  newline
  putBoard board
  newline
  if win then do
    putStr "Player "
    putStr (show winner)
    putStrLn " wins!"
  else if moves_left board then do
      putStr "It is player "
      putStr (show player)
      putStrLn "'s turn!"
      col <- getDigit "Enter C: "
      row <- getDigit "Enter R: "
      if valid_move row col board then
        ttt (next player) (move row col player board)
      else do
        putStrLn "Error: Invalid Move!"
        ttt player board
  else putStrLn "Cat's game!"

