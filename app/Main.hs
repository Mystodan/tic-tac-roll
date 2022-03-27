module Main where

import Data.Time ( getCurrentTime, UTCTime(utctDayTime) )
import GameHandler (
    printBoard,
    readInn,
    updateBoard,
    roll,
    createBoard,
    checkBoard,
    gameLoopErrHandlr,
    getPlayerTurn,
    genRandNum,
    checkDraw
  )
import ComputerOperatedPlayer (
    readCOP,
    copHandleRoll,
    copGetLegalSpace,
    copErr
  )


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
    putStrLn $ printBoard newBoard
    if fst cop then (do
      case rotation of
        "right" -> putStrLn ">>YOU -> Turning right..."
        "left" -> putStrLn  ">>YOU -> Turning left..."
        _-> putStr "") else (do
      case rotation of
        "right" -> putStrLn $ ">> ["++newMark:"] -> Turning right..."
        "left" -> putStrLn  $ ">> ["++newMark:"] -> Turning left..."
        _-> putStr "")
    gameLoop (roll newBoard rotation) nextTurn cop 0
  else do
    gamePrompt "!! Space is occupied!"
    gameLoop (roll board rotation) newMark cop 0
  where
    (newMark, nextTurn) = getPlayerTurn pTurn
    newBoard = updateBoard board (loc, newMark)

playerTurnPrompt ::[Char] -> Char -> (Bool,Char) -> IO String
playerTurnPrompt board turn cop = do
  if not $ fst cop then
   gamePrompt $ ">>> "++ turn : " - Turn"
  else
    gamePrompt $ ">>> Your("++ turn : ") Turn"
  putStrLn "\tIn order to make a move, write it as if it were coordinates on a map:\n \t\t x y , for example: 1 2\n"
  putStrLn $ printBoard board
  getLine

handlePlayer :: [Char]-> Char-> (Bool,Char) -> IO()
handlePlayer board turn cop = do
  inn <- playerTurnPrompt board turn cop
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
  gamePrompt $ ">>> COP("++ turn : ") Turn"
  putStrLn "\tComputer Operated Player\n"
  putStrLn $ printBoard board
  putStrLn $ printBoard newBoard
  case turnHandler of
    "right" -> putStrLn ">>COP -> Turning right..."
    "left" -> putStrLn  ">>COP -> Turning left..."
    _-> putStr ""
  gameLoop (roll newBoard turnHandler ) nextTurn cop seed
  where
    (newMark, nextTurn) = getPlayerTurn $ snd cop
    (min, max) = (0::Int,8::Int)
    turnHandler = copHandleRoll seed
    loc =  copGetLegalSpace board (min,max,seed)
    newBoard = updateBoard board (loc, newMark)



handleCOP :: [Char]-> Char-> (Bool,Char)-> Int -> IO()
handleCOP board turn cop seed = do
  if (turn == 'X') || (turn == 'O') then (do -- handles Computer Operated Player turn
    copTurnHandler board turn cop seed) else (do
    putStrLn "$!¤? FATAL ERROR <<<")

playerAssignmentPrompt :: IO String
playerAssignmentPrompt = do
  gamePrompt "Do you want to play against a Computer Operated Player?\n\tInput either yes and the symbol you want, or no\n \t\t for example: YES X, yes o, no,NO"
  getLine

handleWinMsg:: Char -> (Bool,Char) -> IO ()
handleWinMsg winner cop = do
  if fst cop then
    if snd cop == winner then
      gamePrompt "!COP - The Computer Operated Player WON!"
    else
      gamePrompt "!YOU - You WON!"
  else
    gamePrompt $ "!Congrats [" ++ winner: "] for winning!"



gameLoop::[Char]-> Char-> (Bool,Char) -> Int ->IO()
gameLoop board turn cop seed = do
  if not winCondition then do  -- win condition checks recursively
    if not (checkDraw board 0) then do
      putStrLn $ "["++board++"]"
      if turn /= snd cop then do
        handlePlayer board turn cop
      else do
        handleCOP board turn cop seed
    else do
      putStrLn $ printBoard board
      gamePrompt "!DRAW!"
  else do
    putStrLn $ "["++board++"]"
    putStrLn $ printBoard winningBoard
    handleWinMsg winner cop
  where
    (winCondition,winner,winningBoard) = checkBoard board -- handles winning condition

startGameLoop :: Int -> IO()
startGameLoop seed = do
  gameType <- playerAssignmentPrompt
  let copData = readCOP gameType
  if copData /= copErr  then
    gameLoop newBoard 'X' copData seed
  else
    startGameLoop seed
  where
    newBoard = createBoard

main :: IO ()
main = do
  currTime<- getCurrentTime
  let seed = floor $ utctDayTime currTime ::Int
  startGameLoop seed
    