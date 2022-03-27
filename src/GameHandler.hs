module GameHandler(
      updateBoard,
      countOcc,
      createBoard,
      convertInn,
      roll,
      printBoard,
      swapEarlyMarks,
      checkBoard,
      readInn,
      readUnit,
      getPlayerTurn,
      gameLoopErrHandlr,
      genRandNum,
      checkDraw
    ) where

import Data.List.Split (splitOn)
import Data.Char(toLower)
import Data.Type.Equality (inner)
import System.Random (randomR, mkStdGen)

-- Pure game logic

-- | countOcc
--    counts occurances of an element from list
-- Examples:
--
-- >>> countOcc 'J' "Jason, Jason Bourne" 
-- 2
-- >>>  countOcc 'B' "BOB DYLAN IS OP"
-- 2
-- >>>  countOcc '!' ""
-- 0
countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list


-- | createBoard
--    sets a new board
-- Examples:
--
-- >>> createBoard 
-- "         " 
createBoard::[Char]
createBoard = [
  ' ',' ',' ',
  ' ',' ',' ',
  ' ',' ',' '
  ]


-- | updateBoard
--    Updates the board by replacing a value on the new board
--    (x:xs): the original board it takes inn(rawdata)
-- Examples:
--
-- >>> updateBoard createBoard (0,'X')
-- "X        "
-- >>> updateBoard "O X OXX  " (0,'X')
-- "X X OXX  "
updateBoard :: [Char] -> (Int, Char) -> [Char]
updateBoard [] _ = []
updateBoard (_:xs) (0,a) = a:xs
updateBoard (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: updateBoard xs (n-1,a)

-- | printBoard
--    converts raw board data to printable kawaii string
-- Examples:
--
-- >>> printBoard createBoard
-- " \t   1  2  3 \n#\t1 [ ][ ][ ]\n#\t2 [ ][ ][ ]\n#\t3 [ ][ ][ ]"
-- >>> printBoard "O X OXX  "
-- " \t   1  2  3 \n#\t1 [O][ ][X]\n#\t2 [ ][O][X]\n#\t3 [X][ ][ ]"
printBoard::[Char]->String
printBoard inn = [
  ' ','\t',' ',' ',' ','1',' ',' ','2',' ',' ','3',' ','\n',
  '#','\t','1',' ','[',inn!!0,']','[',inn!!1,']','[',inn!!2,']','\n',
  '#','\t','2',' ','[',inn!!3,']','[',inn!!4,']','[',inn!!5,']','\n',
  '#','\t','3',' ','[',inn!!6,']','[',inn!!7,']','[',inn!!8,']'
  ]


-- | swapEarlyMarks
--    "swaps the first and third mark on board
--    inn: the original board it takes inn(rawdata)
-- Examples:
--
-- >>> swapEarlyMarks createBoard 
-- "         "
-- >>> swapEarlyMarks "O X OXX  " 
-- "X O OXX  "
swapEarlyMarks::[Char] -> [Char]
swapEarlyMarks inn = [
  inn!!2,inn!!1,inn!!0,
  inn!!3,inn!!4,inn!!5,
  inn!!6,inn!!7,inn!!8
  ]

-- | roll
-- Rotates the board in a direction after swapping the 0th and 2nd mark:
-- inn: the original board it takes inn(rawdata)
-- dir: the direction of the rotation
--    
-- Examples:
--
-- >>> roll createBoard "left" 
-- "         "
-- >>> roll "O X OXX  " "right"
-- "X X O  XO"
-- >>> roll "O X OXX  " "left"
-- "OX  O X X"
roll:: [Char] -> String ->  [Char]
roll inn dir = do
  let new = swapEarlyMarks inn
  case dir of
    "left" ->   -- rotates board to the left
      [
        new!!2,new!!5,new!!8,
        new!!1,new!!4,new!!7,
        new!!0,new!!3,new!!6
        ]
    "right" ->  -- rotates board to the right
      [
        new!!6,new!!3,new!!0,
        new!!7,new!!4,new!!1,
        new!!8,new!!5,new!!2
        ]
    _ -> inn

-- | checkDraw
-- checks board for draw
-- >>> checkDraw createBoard 0 
-- False
-- >>> checkDraw "OXOXOXXOX" 0 
-- True
-- >>> checkDraw createBoard 0 
-- False
checkDraw:: [Char]->Int->Bool
checkDraw board index = index >= length board || (do (board!!index /= ' ') && checkDraw board (index+1))





-- | checkBoard
-- Checks board for win condition and returns:
-- if someone won  ::Bool
-- who won         ::Char
-- winning board   ::[Char] -- after drawing a line through the winning tiles (rawdata)
-- >>>  checkBoard createBoard
-- (False,' ',"")
-- >>> checkBoard "OXOXOXXOX"
-- (False,' ',"")
-- >>> checkBoard "O X OXX X"
-- (True,'X',"O | O|X |")
checkBoard:: [Char] -> (Bool,Char,[Char])
checkBoard inn
  |   inn!!0 /= ' ' && inn!!0 == inn!!1 && inn!!0 == inn!!2 = (True,inn!!0,[
      '-','-','-',
      inn!!3,inn!!4,inn!!5,
      inn!!6,inn!!7,inn!!8
  ])
  |   inn!!3 /= ' ' && inn!!3 == inn!!4 && inn!!3 == inn!!5 = (True,inn!!3,[
      inn!!0,inn!!1,inn!!2,
      '-','-','-',
      inn!!6,inn!!7,inn!!8
  ])
  |   inn!!6 /= ' ' && inn!!6 == inn!!7 && inn!!6 == inn!!8 = (True,inn!!6,[
      inn!!0,inn!!1,inn!!2,
      inn!!3,inn!!4,inn!!5,
     '-','-','-'
  ])
  |   inn!!0 /= ' ' && inn!!0 == inn!!3 && inn!!0 == inn!!6 = (True,inn!!0,[
      '|',inn!!1,inn!!2,
      '|',inn!!4,inn!!5,
      '|',inn!!7,inn!!8
  ])
  |   inn!!1 /= ' ' && inn!!1 == inn!!4 && inn!!1 == inn!!7 = (True,inn!!1,[
      inn!!0,'|',inn!!2,
      inn!!3,'|',inn!!5,
      inn!!6,'|',inn!!8
  ])
  |   inn!!2 /= ' ' && inn!!2 == inn!!5 && inn!!2 == inn!!8 = (True,inn!!2,[
      inn!!0,inn!!1,'|',
      inn!!3,inn!!4,'|',
      inn!!6,inn!!7,'|'
  ])
  |   inn!!0 /= ' ' && inn!!0 == inn!!4 && inn!!0 == inn!!8 = (True,inn!!0,[
      '\\',inn!!1,inn!!2,
      inn!!3,'\\',inn!!5,
      inn!!6,inn!!7,'\\'
  ])
  |   inn!!2 /= ' ' && inn!!2 == inn!!4 && inn!!2 == inn!!6 = (True,inn!!2,[
      inn!!0,inn!!1,'/',
      inn!!3,'/',inn!!5,
      '/',inn!!7,inn!!8
  ])
  | otherwise = (False,' ',[])

type Unit = String

-- | readUnit
-- reads string as Int, and returns err if anything other than int
-- >>> readUnit "1"  
-- 1
-- >>> readUnit "1 12311"
-- 1
-- >>> readUnit "1 12311a"  
-- 1
readUnit :: String -> Int
readUnit s = case reads s of
    (n, ' ':unit):_ ->  n
    (n, ""      ):_ ->  n
    _ -> 13 -- Err msg

-- |getPlayerTurn
-- Gets player turn and returns marks
-- turn: player turn
-- >>>  getPlayerTurn 'X'  
-- ('X','O')  
-- >>>  getPlayerTurn 'O'  
-- ('O','X') 
getPlayerTurn :: Char -> (Char, Char)
getPlayerTurn turn = do
  if turn == 'O' then -- if current player is O
    ('O','X')         -- returns current mark and next mark
  else                -- if current player is X
    ('X','O')         -- returns current mark and next mark

-- | gameLoopErrHandlr
-- Gets raw location data for board, and checks for err
-- err: raw location data for board
-- >>> gameLoopErrHandlr 11 
-- True  
-- >>>  gameLoopErrHandlr 12 
-- True  
-- >>>  gameLoopErrHandlr 13 
-- True 
-- >>>  gameLoopErrHandlr 1  
-- False
gameLoopErrHandlr :: Int -> Bool
gameLoopErrHandlr err = do
  (err == 11 || err == 12 || err == 13) && (do --different err codes
    True) -- returns true since err has been encountered

-- |convertInn
-- Converts input to data nessescary for board. and checks for err
-- inn: data from getLine as String 
-- >>> convertInn ["1","2"] 
-- (1,2,"2") 
-- >>> convertInn ["1","2", "right"]  
-- (1,2,"right")
-- >>> convertInn ["a","a"]  
-- (13,13,"a")
convertInn :: [String] -> (Int,Int,String)
convertInn inn = do
  let (
        first,  -- gets first value of string array as Int 
        second, -- gets second value of string array as Int 
        last    -- gets last value of string array as String 
        ) = (
          readUnit $ inn!!0::Int,
          readUnit $ inn!!1::Int,
          map toLower (inn!!(length inn-1))
          )
  (first,second,last)

-- | genRandNum
-- >>>  genRandNum 1 2 1  
-- 2  
-- >>>  genRandNum 1 2 10  
-- 1   
genRandNum:: Int -> Int -> Int -> Int
genRandNum min max seed = retNum
  where 
    (retNum,_) = randomR (min,max) $ mkStdGen seed

-- | readInn
-- Checks if string is legal and returns raw location data(an index number of board) for board
-- >>>  readInn "1 2" 
-- (3,"")  
-- >>>  readInn "1 3 right"  
-- (6,"right")
-- >>>  readInn "1 3 3 right"  
-- (11,"")
-- >>>  readInn "9 9 right"  
-- (12,"")
readInn::String -> (Int,String)
readInn inn = do
  let list = splitOn " " inn  --                        // Splits String into list of strings 
  let (
        val1, -- = readUnit $ list!!0::Int,             // Val1, first value of list is read as an int
        val2, -- = readUnit $ list!!1::Int,             // Val2, second value is read as an int
        gLast -- = map toLower (list!!(length list-1))  // gLast converts the last value to lowercase in order to get rotation 
        ) = convertInn list

  if val1 /= 13 || val2 /= 13 then do -- if a specific err hasnt been encountered
    if length list == 2 then do  -- if no
      if val2 == 0 || val1 == 0 || val1 > 3 || val2 > 3 then -- checks if numbers are legal
        (12,"") else -- Err msg
          (3*(val2-1) + (val1-1),"")        -- converts and returns coordinates to location in list
    else if length list == 3 then do
      if val2 == 0 || val1 == 0 || val1 > 3 || val2 > 3 then
        (12,"") else -- Err msg
          if gLast == "right" then          -- checks inputted rotation
            (3*(val2-1) + (val1-1),"right")  -- converts and returns coordinates to location in list and rotation
          else if gLast == "left" then      -- checks inputted rotation
            (3*(val2-1) + (val1-1),"left")   -- converts and returns coordinates to location in list and rotation
          else
            (3*(val2-1) + (val1-1),"")       -- converts and returns coordinates to location in list
    else
      (11,"")        -- Err msg
  else
      (13,"")        -- Err msg
  