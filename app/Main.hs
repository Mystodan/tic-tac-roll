module Main where

import Lib (
  printBoard,
  readInn,
  updateBoard,
  roll,
  newBoard,
  checkBoard)

gameLoop::[Char]-> Char->IO()
gameLoop board turn = do
  let (winCon,winner) = checkBoard board
  if not winCon then do
    putStrLn "-------------------------------------------------------------------------"
    putStrLn $ ">>> "++ turn : " - Turn"
    putStrLn "-------------------------------------------------------------------------"
    putStrLn "In order to make a move, write it as if it were coordinates on a map:\n \t x y , for example: 1 2\n"
    putStrLn $ printBoard board
    inn <- getLine
    let (tInn,rotState) = readInn inn
    if tInn == 11 then do
      putStrLn "-------------------------------------------------------------------------"
      putStrLn "!!! Wrong amount of numbers, please input (x y) where the size is between 1-3"
      putStrLn "-------------------------------------------------------------------------"
      gameLoop board turn
    else if tInn == 12 then do
      putStrLn "-------------------------------------------------------------------------"
      putStrLn "!!! Wrong size of numbers, please input (x y) where the size is between 1-3"
      putStrLn "-------------------------------------------------------------------------"
      gameLoop board turn
    else do
      if turn == 'X' then do
        if board!!tInn /= 'O' then do
          let addPiece = updateBoard board (tInn, 'X')
          gameLoop (roll addPiece rotState) 'O'
        else do
          putStrLn "-------------------------------------------------------------------------"
          putStrLn "!! Space is occupied!"
          putStrLn "-------------------------------------------------------------------------"
          gameLoop (roll board rotState) 'X'

      else if turn == 'O' then do
        if board!!tInn /= 'X' then do
          let addPiece = updateBoard board (tInn, 'O')
          gameLoop (roll addPiece rotState) 'X'
        else do
          putStrLn "-------------------------------------------------------------------------"
          putStrLn "!! Space is occupied!"
          putStrLn "-------------------------------------------------------------------------"
          gameLoop (roll board rotState) 'O'
      else do
        putStrLn "$!¤? FATAL ERROR <<<"
  else do
    putStrLn $ printBoard board
    putStrLn $ "Congrats " ++ winner: " for winning!"
main :: IO ()
main = do
  gameLoop newBoard 'X'
