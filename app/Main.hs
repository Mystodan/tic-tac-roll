module Main where

import Lib (newBoard, gameLoop)

main :: IO ()
main = do
  gameLoop newBoard
