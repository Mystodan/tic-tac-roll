module Lib
    (updateBoard,
    countOcc,
    newBoard,
    roll,
    printBoard,
    checkBoard,
    readInn,
    readUnit,
    getPlayerTurn) where

import Data.List.Split (splitOn)
import Data.Char(toLower)

countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list

newBoard::[Char]
newBoard = [
  ' ',' ',' ',
  ' ',' ',' ',
  ' ',' ',' '
  ]

updateBoard :: [Char] -> (Int, Char) -> [Char]
updateBoard [] _ = []
updateBoard (_:xs) (0,a) = a:xs
updateBoard (x:xs) (n,a) =
  if n < 0
    then x:xs
    else x: updateBoard xs (n-1,a)

printBoard::[Char]->String
printBoard inn = [
  ' ',' ',' ',' ',' ','1',' ',' ','2',' ',' ','3',' ','\n',
  '#',' ','1',' ','[',inn!!0,']','[',inn!!1,']','[',inn!!2,']','\n',
  '#',' ','2',' ','[',inn!!3,']','[',inn!!4,']','[',inn!!5,']','\n',
  '#',' ','3',' ','[',inn!!6,']','[',inn!!7,']','[',inn!!8,']','\n'
  ]


roll:: [Char] -> String ->  [Char]
roll  inn dir
  | dir == "left" =
    [
      inn!!0,inn!!5,inn!!8,
      inn!!1,inn!!4,inn!!7,
      inn!!2,inn!!3,inn!!6
      ]
  | dir == "right" =
    [
      inn!!6,inn!!3,inn!!2,
      inn!!7,inn!!4,inn!!1,
      inn!!8,inn!!5,inn!!0
      ]
  | otherwise = inn


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
  |   inn!!6 /= ' ' && inn!!6 == inn!!7 && inn!!6 == inn!!8 = (True,inn!!2,[
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

readUnit :: String -> Int
readUnit s = case reads s of               
    (n, ' ':unit):_ ->  n 
    (n, ""      ):_ ->  n  
    _ -> 13 -- Err msg

getPlayerTurn :: Char -> (Char, Char)
getPlayerTurn turn = do
  if turn == 'O' then
    ('O','X')
  else
    ('X','O')

readInn::String -> (Int,String)
readInn inn = do
  let (gArr,val1,val2,gLast) = (splitOn " " inn, readUnit $ gArr!!0::Int,readUnit $ gArr!!1::Int,map toLower (gArr!!(length gArr-1)))
  if val1 /= 13 || val2 /= 13 then do
    if length gArr == 2 then do
      if val2 == 0 || val1 == 0 || val1 > 3 || val2 > 3 then
        (12,"") else -- Err msg
          (3*(val2-1) + (val1-1),"")
    else if length gArr == 3 then do
      if val2 == 0 || val1 == 0 || val1 > 3 || val2 > 3 then
        (12,"") else -- Err msg
          if gLast == "right" then
            (3*(val2-1) + (val1-1),"right")
          else if gLast == "left" then
            (3*(val2-1) + (val1-1),"left")
          else
            (3*(val2-1) + (val1-1),"")
    else
      (11,"") -- Err msg
  else
    (13,"")  -- Err msg
  