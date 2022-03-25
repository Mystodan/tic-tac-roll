module Main where

import Lib (
  printBoard,
  readInn,
  updateBoard,
  roll,
  newBoard,
  checkBoard,
  getPlayerTurn)

gamePrompt::String->IO()
gamePrompt inn = do
    putStrLn "-------------------------------------------------------------------------"
    putStrLn inn
    putStrLn "-------------------------------------------------------------------------"

handleErr::Int->IO()
handleErr err = case err of
  11 -> do
    gamePrompt "!!! Wrong amount of numbers, please input (x y) where the size is between 1-3"
  12 -> do
    gamePrompt "!!! Wrong size of numbers, please input (x y) where the size is between 1-3"
  13 -> do
    gamePrompt "!!! Wrong input, please input (x y) where x y is a number(1-3)"
  _ -> putStr ""

playerTurnHandler ::[Char]-> (Int,String)-> Char -> Char -> IO()
playerTurnHandler board (loc,rotation) pTurn wantPiece = do
  let (newPiece, nextTurn) = getPlayerTurn pTurn
  if wantPiece /= nextTurn then do
    let addPiece = updateBoard board (loc, newPiece)
    gameLoop (roll addPiece rotation) nextTurn
  else do
    gamePrompt "!! Space is occupied!"
    gameLoop (roll board rotation) newPiece

gameLoop::[Char]-> Char->IO()
gameLoop board turn = do
  let (winCondition,winner,winningBoard) = checkBoard board
  if not winCondition then do
    gamePrompt $ ">>> "++ turn : " - Turn"
    putStrLn "In order to make a move, write it as if it were coordinates on a map:\n \t x y , for example: 1 2\n"
    putStrLn $ printBoard board
    inn <- getLine
    let (tInn,rotation) = readInn inn
    if tInn == 11 || tInn == 12 || tInn == 13 then do
      handleErr tInn
      gameLoop board turn
    else do
      if (turn == 'X') || (turn == 'O') then (do
        playerTurnHandler board (tInn,rotation) turn (board!!tInn)) else (do
        putStrLn "$!¤? FATAL ERROR <<<")
  else do
    putStrLn $ printBoard winningBoard
    gamePrompt $ "!Congrats [" ++ winner: "] for winning!"

main :: IO ()
main = gameLoop newBoard 'X'