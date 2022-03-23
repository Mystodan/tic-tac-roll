module Lib
    (updateBoard,
    gameLoop,
    countOcc,
    getVal,
    newBoard,
    roll,
    printBoard,
    checkBoard
    ) where

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
  '#','[',inn!!0,']','[',inn!!1,']','[',inn!!2,']','\n',
  '#','[',inn!!3,']','[',inn!!4,']','[',inn!!5,']','\n',
  '#','[',inn!!6,']','[',inn!!7,']','[',inn!!8,']'
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
  

checkBoard:: [Char] -> Bool 
checkBoard inn 
  |    (inn!!0 /= ' ' && inn!!0 == inn!!1 && inn!!0 == inn!!2) 
  ||   (inn!!3 /= ' ' && inn!!3 == inn!!4 && inn!!3 == inn!!5) 
  ||   (inn!!2 /= ' ' && inn!!2 == inn!!5 && inn!!2 == inn!!8) = True 
  |    (inn!!0 /= ' ' && inn!!0 == inn!!3 && inn!!0 == inn!!6) 
  ||   (inn!!1 /= ' ' && inn!!1 == inn!!4 && inn!!1 == inn!!7) 
  ||   (inn!!2 /= ' ' && inn!!2 == inn!!5 && inn!!2 == inn!!8) = True
  |    (inn!!0 /= ' ' && inn!!0 == inn!!4 && inn!!0 == inn!!8) 
  ||   (inn!!2 /= ' ' && inn!!2 == inn!!4 && inn!!2 == inn!!6) = True
  | otherwise = False


gameLoop::[Char]->IO()
gameLoop map = do
  putStrLn $ printBoard map