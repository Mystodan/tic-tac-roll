module GameHandler(
      updateBoard,
      countOcc,
      newBoard,
      roll,
      printBoard,
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
{-
- counts occurances of an element from list
-}
countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list

{- 
- sets a new board
-}
newBoard::[Char]
newBoard = [
  ' ',' ',' ',
  ' ',' ',' ',
  ' ',' ',' '
  ]

{- 
- Updates the board by replacing a value on the new board
- (x:xs): the original board it takes inn(rawdata)
-}
updateBoard :: [Char] -> (Int, Char) -> [Char]
updateBoard [] _ = []
updateBoard (_:xs) (0,a) = a:xs
updateBoard (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: updateBoard xs (n-1,a)

{- 
- Converts raw list data to printable string
- inn: the original board it takes inn(rawdata)
-}
printBoard::[Char]->String
printBoard inn = [
  ' ','\t',' ',' ',' ','1',' ',' ','2',' ',' ','3',' ','\n',
  '#','\t','1',' ','[',inn!!0,']','[',inn!!1,']','[',inn!!2,']','\n',
  '#','\t','2',' ','[',inn!!3,']','[',inn!!4,']','[',inn!!5,']','\n',
  '#','\t','3',' ','[',inn!!6,']','[',inn!!7,']','[',inn!!8,']'
  ]

{- 
- Swaps the 0th and 2nd mark:
- inn: the original board it takes inn(rawdata)
-}
swapEarlyMarks::[Char] -> [Char]
swapEarlyMarks inn = [
  inn!!2,inn!!1,inn!!0,
  inn!!3,inn!!4,inn!!5,
  inn!!6,inn!!7,inn!!8
  ]

{- 
- Rotates the board in a direction after swapping the 0th and 2nd mark:
- inn: the original board it takes inn(rawdata)
- dir: the direction of the rotation
-}
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

checkDraw:: [Char]->Int->Bool
checkDraw board index = do
  index >= length board || (do
    (board!!index /= ' ') && checkDraw board (index+1))



{- 
- Checks board for win condition and returns:
- if someone won  ::Bool
- who won         ::Char
- winning board   ::[Char] -- after drawing a line through the winning tiles (rawdata)
-}
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

{- 
- reads string as Int, and returns err if anything other than int
-}
readUnit :: String -> Int
readUnit s = case reads s of
    (n, ' ':unit):_ ->  n
    (n, ""      ):_ ->  n
    _ -> 13 -- Err msg

{- 
- Gets player turn and returns marks
- turn: player turn
-}
getPlayerTurn :: Char -> (Char, Char)
getPlayerTurn turn = do
  if turn == 'O' then -- if current player is O
    ('O','X')         -- returns current mark and next mark
  else                -- if current player is X
    ('X','O')         -- returns current mark and next mark

{- 
- Gets raw location data for board, and checks for err
- err: raw location data for board
-}
gameLoopErrHandlr :: Int -> Bool
gameLoopErrHandlr err = do
  (err == 11 || err == 12 || err == 13) && (do --different err codes
    True) -- returns true since err has been encountered

{- 
- Converts input to data nessescary for board.
- inn: data from getLine as String 
-}
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

genRandNum:: Int -> Int -> Int -> Int
genRandNum min max seed = do
  let (retNum,_) = randomR (min,max) $ mkStdGen seed
  retNum

{-
 - Checks if data is legal and returns nessescary data 
 -}
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
  