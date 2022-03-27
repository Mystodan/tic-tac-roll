module Main where

import Data.Time ( getCurrentTime, UTCTime(utctDayTime) )
import ComputerOperatedPlayer ( 
    readCOP,
    copHandleRoll,
    copGetLegalSpace 
  )
import GameHandler (
    printBoard,
    readInn,
    updateBoard,
    roll,
    newBoard,
    checkBoard,
    gameLoopErrHandlr,
    getPlayerTurn,
    genRandNum,
    checkDraw
  )
import Data.String (String)



gamePrompt::String->IO()
gamePrompt inn = do
    putStrLn "--------------------------------------------------------------------------------"
    putStrLn $ "\t"++ inn
    putStrLn "--------------------------------------------------------------------------------"


handleErr::Int->IO()
handleErr err = case err of
  11 ->
    gamePrompt "!!! Wrong amount of numbers, please input (x y) where the size is between 1-3"
  12 ->
    gamePrompt "!!! Wrong size of numbers, please input (x y) where the size is between 1-3"
  13 ->
    gamePrompt "!!! Wrong input, please input (x y) where x y is a number(1-3)"
  _ -> putStr ""

playerTurnHandler ::[Char]-> (Int,String)-> Char -> Char -> (Bool,Char) ->IO()
playerTurnHandler board (loc,rotation) pTurn wantMark cop = do
  if wantMark /= nextTurn then do -- checks if space is occupied
    gameLoop (roll newBoard rotation) nextTurn cop 0
  else do
    gamePrompt "!! Space is occupied!"
    gameLoop (roll board rotation) newMark cop 0
  where
    (newMark, nextTurn) = getPlayerTurn pTurn
    newBoard = updateBoard board (loc, newMark)

playerTurnPrompt ::[Char] -> Char -> IO String
playerTurnPrompt board turn = do
  gamePrompt $ ">>> "++ turn : " - Turn"
  putStrLn "\tIn order to make a move, write it as if it were coordinates on a map:\n \t\t x y , for example: 1 2\n"
  putStrLn $ printBoard board
  getLine

handlePlayer :: [Char]-> Char-> (Bool,Char) -> IO()
handlePlayer board turn cop = do
  inn <- playerTurnPrompt board turn
  let (tInn,rotation) = readInn inn
  if gameLoopErrHandlr tInn then do -- handles err
    handleErr tInn
    gameLoop board turn cop 0
  else do
    if (turn == 'X') || (turn == 'O') then (do -- handles player turn
      playerTurnHandler board (tInn,rotation) turn (board!!tInn) cop) else (do
      putStrLn "$!¤? FATAL ERROR <<<")



copTurnHandler:: [Char]-> Char-> (Bool,Char) -> Int->IO()
copTurnHandler board turn cop seed= do

  gameLoop (roll newBoard $ copHandleRoll seed) nextTurn cop seed
  where
    (newMark, nextTurn) = getPlayerTurn $ snd cop
    (min, max) = (0::Int,8::Int)
    loc =  copGetLegalSpace board (min,max,seed)
    newBoard = updateBoard board (loc, newMark)



handleCOP :: [Char]-> Char-> (Bool,Char)-> Int -> IO()
handleCOP board turn cop seed = do
  if (turn == 'X') || (turn == 'O') then (do -- handles player turn
    copTurnHandler board turn cop seed) else (do
    putStrLn "$!¤? FATAL ERROR <<<")

playerAssignmentPrompt :: IO String
playerAssignmentPrompt = do
  gamePrompt "Do you want to play against a Computer Operated Player?\n\tInput either yes and the symbol you want, or no\n \t\t for example: YES X, yes o, no,NO"
  getLine

gameLoop::[Char]-> Char-> (Bool,Char) -> Int ->IO()
gameLoop board turn cop seed = do
  if not winCondition then do  -- win condition checks recursively
    if not (checkDraw board 0) then do
      if turn /= snd cop then do
        handlePlayer board turn cop
      else do
        handleCOP board turn cop seed
    else do
      putStrLn $ printBoard board
      gamePrompt "!DRAW!"
  else do
    putStrLn $ printBoard winningBoard
    gamePrompt $ "!Congrats [" ++ winner: "] for winning!"
  where
    (winCondition,winner,winningBoard) = checkBoard board -- handles winning condition

startGameLoop :: Int -> IO()
startGameLoop seed = do
  gameType <- playerAssignmentPrompt
  let copData = readCOP gameType
  if copData ==  (True, 'O') || copData ==  (True, 'X') || copData ==  (False,'+') then
    gameLoop newBoard 'X' copData seed
  else
    startGameLoop seed

main :: IO ()
main = do
  currTime<- getCurrentTime
  let seed = floor $ utctDayTime currTime ::Int
  startGameLoop seed
    