module ComputerOperatedPlayer(
    readStr,
    readCOP,
    copHandleRoll,
    copGetLegalSpace,
    copErr
  )where
import Data.List.Split ( splitOn )
import Data.Type.Equality (inner)
import Data.Char (toLower)
import GameHandler (
    genRandNum,
    createBoard,            
    checkDraw)

-- | readStr
-- >>> readStr "1 2" 
-- ("1","2")
readStr :: String -> (String,String)
readStr inn = do
  let list = splitOn " " inn
  (list!!0, list!!(length list-1))

-- | copErr
-- >>> copErr 
-- (False,'-')
copErr::(Bool,Char)
copErr = (False, '-')

-- | readCOP
-- >>> readCOP "mongobongo"  
-- (False,'-')
-- >>>readCOP "no" 
-- (False,'+')
-- >>>readCOP "yes x" 
-- (True,'O')
-- >>>readCOP "YES O" 
-- (True,'X')
-- >>>readCOP "yes" 
-- (False,'-')
-- >>>readCOP "yes mom" 
-- (False,'-')
readCOP::String -> (Bool,Char)
readCOP inn = do
  let ((t1,t2), first, second) = (readStr inn,map toLower t1, map toLower t2)
  case first of
    "yes" -> do
      if second == "x" then
        (True,'O')
      else if second == "o" then
        (True,'X')
      else
        copErr

    "no" -> (False,'+')
    _ -> copErr

-- | copGetLegalSpace
-- >>> copGetLegalSpace createBoard (0,8,1) 
-- 5
-- >>> copGetLegalSpace "XXXXXXXX " (0,8,1) 
-- 8
-- >>> copGetLegalSpace "XXXXXXXXX" (0,8,1) 
-- -1
copGetLegalSpace:: [Char]-> (Int, Int,Int) -> Int
copGetLegalSpace board (min,max,seed) = do
  if not (checkDraw board 0) then do
    let  rnd  = genRandNum min max seed
    if board!!rnd /= ' ' then  copGetLegalSpace board (min,max,seed +5)
    else rnd
  else -1

-- | copHandleRoll
-- >>> copHandleRoll 1 
-- "right"
-- >>> copHandleRoll 6 
-- "left"
-- >>> copHandleRoll 2 
-- ""
copHandleRoll ::Int -> String 
copHandleRoll seed = do
  case rnd of
    0 -> "left"
    1 -> "right"
    _->  ""
  where
    rnd  = genRandNum 0 3 seed



