module ComputerOperatedPlayer(
    readCOP,
    copHandleRoll,
    copGetLegalSpace
  )where
import Data.List.Split ( splitOn )
import Data.Type.Equality (inner)
import Data.Char (toLower)
import GameHandler (genRandNum)

readStr :: String -> (String,String)
readStr inn = do
  let list = splitOn " " inn
  (list!!0, list!!(length list-1))

noAI::(Bool,Char)
noAI = (False, '-')
readCOP::String -> (Bool,Char)
readCOP inn = do
  let ((t1,t2), first, second) = (readStr inn,map toLower t1, map toLower t2)
  case first of
    "yes" -> do
      if head second == 'x' then
        (True,'O')
      else if head second == 'o' then
        (True,'X')
      else
        noAI

    "no" -> (False,'+')
    _ -> (False, '-')

copGetLegalSpace:: [Char]-> (Int, Int,Int) -> Int
copGetLegalSpace board (min,max,seed) = do
  let  rnd  = genRandNum min max seed
  if board!!rnd /= ' ' then  copGetLegalSpace board (min,max,seed +5)
  else rnd
   

copHandleRoll ::Int -> String 
copHandleRoll seed = do
  if rnd < 2 then
    "left"
  else
    "right"
  where
    rnd  = genRandNum 0 4 seed



