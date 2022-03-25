module Lib
    (updateBoard,
    countOcc,
    getVal,
    newBoard,
    roll,
    printBoard,
    checkBoard,
    readInn) where

import Data.List.Split (splitOn)
import Data.Char(toLower)

countOcc :: Eq a => a -> [a] -> Int
countOcc want [] = 0
countOcc want list = sum $
 map(const 1) $
 filter (==want) list

getVal :: Int -> [a] -> [a]
getVal = take

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
  '#',' ','3',' ','[',inn!!6,']','[',inn!!7,']','[',inn!!8,']'
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


checkBoard:: [Char] -> (Bool,Char)
checkBoard inn
  |   inn!!0 /= ' ' && inn!!0 == inn!!1 && inn!!0 == inn!!2 = (True,inn!!0)
  |   inn!!3 /= ' ' && inn!!3 == inn!!4 && inn!!3 == inn!!5 = (True,inn!!3)
  |   inn!!6 /= ' ' && inn!!6 == inn!!7 && inn!!6 == inn!!8 = (True,inn!!2)
  |   inn!!0 /= ' ' && inn!!0 == inn!!3 && inn!!0 == inn!!6 = (True,inn!!0)
  |   inn!!1 /= ' ' && inn!!1 == inn!!4 && inn!!1 == inn!!7 = (True,inn!!1)
  |   inn!!2 /= ' ' && inn!!2 == inn!!5 && inn!!2 == inn!!8 = (True,inn!!2)
  |   inn!!0 /= ' ' && inn!!0 == inn!!4 && inn!!0 == inn!!8 = (True,inn!!0)
  |   inn!!2 /= ' ' && inn!!2 == inn!!4 && inn!!2 == inn!!6 = (True,inn!!2)
  | otherwise = (False,' ')


readInn::String -> (Int,String)
readInn inn = do
  let arr = map read $ words inn :: [Int]
  let gArr = splitOn " " inn
  let gLast = map toLower (gArr!!(length gArr-1))
  if length arr == 2 then do
    if arr!!1 == 0 || arr!!0 == 0 || arr!!0 > 3 || arr!!1 > 3 then
      (12,"") else
        (3*(arr!!1-1) + (arr!!0-1),"")
  else if length arr == 3 then do
    if arr!!1 == 0 || arr!!0 == 0 || arr!!0 > 3 || arr!!1 > 3 then
      (12,"") else
        if gLast == "right" then
          (3*(arr!!1-1) + (arr!!0-1),"right")
        else if gLast == "left" then
          (3*(arr!!1-1) + (arr!!0-1),"left")
        else
          (3*(arr!!1-1) + (arr!!0-1),"")
  else
    (11,"")





  