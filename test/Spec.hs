import Test.Hspec
import Control.Exception(evaluate)
import Test.DocTest (doctest)
import Test.HUnit (runTestTT, Test (TestCase, TestLabel, TestList), assertEqual)
import System.Random (mkStdGen, Random (randomR), StdGen)
import Data.Time.Clock (getCurrentTime, utctDayTime)
import GameHandler (
    countOcc,
    convertInn,
    printBoard,
    swapEarlyMarks,
    createBoard,
    readInn,
    readUnit,
    updateBoard,
    roll,
    checkBoard,
    gameLoopErrHandlr,
    getPlayerTurn,
    genRandNum,
    checkDraw
  )
import ComputerOperatedPlayer (
    readStr,
    readCOP,
    copHandleRoll,
    copGetLegalSpace,
    copErr
  )


unitTests :: Test
unitTests = TestList [
  TestLabel "countOcc" $ TestCase $
   assertEqual "counts occurrances of a char from string" 2 $
   countOcc 'J' "Jason, Jason Bourne" ,

  TestLabel "createBoard" $ TestCase $
   assertEqual "creates a new board" "         " createBoard,

  TestLabel "updateBoard" $ TestCase $
   assertEqual "updates board based on index data and char" "X        "
   $ updateBoard createBoard (0,'X'),

  TestLabel "printBoard" $ TestCase $
   assertEqual "converts raw board data to printable kawaii string" " \t   1  2  3 \n#\t1 [O][ ][X]\n#\t2 [ ][O][X]\n#\t3 [X][ ][ ]"
   $ printBoard "O X OXX  ",

  TestLabel "swapEarlyMarks" $ TestCase $
   assertEqual "swaps the first and third mark on board" "X O OXX  "
   $ swapEarlyMarks "O X OXX  ",

  TestLabel "roll" $ TestCase $
   assertEqual "swaps the first and third mark on board then rolls board to a direction" "X X O  XO"
   $ roll "O X OXX  " "right",

  TestLabel "checkDraw" $ TestCase $
   assertEqual "checks board for draw" True
   $checkDraw "OXOXOXXOX" 0 ,

  TestLabel "checkBoard" $ TestCase $
   assertEqual "checks board for win condition" (True,'X',"O | O|X |")
   $ checkBoard "O X OXX X" ,

  TestLabel "readUnit"  $ TestCase $
   assertEqual "reads string and checks for err " 13
   $  readUnit "" ,

  TestLabel "getPlayerTurn" $ TestCase $
   assertEqual "gets current turn and returns current- and next turn" ('X','O')
   $ getPlayerTurn 'X' ,

  TestLabel "gameLoopErrHandlr" $ TestCase $
   assertEqual "Gets raw index data for board, and checks for err" False
   $ gameLoopErrHandlr 1,

  TestLabel "convertInn" $ TestCase $
   assertEqual "Converts input to data nessescary for board. and checks for err" (1,2,"right")
   $ convertInn ["1","2", "right"],

  TestLabel "genRandNum" $ TestCase $
   assertEqual "gets a random num based on min max and seed" 2
   $ genRandNum 1 2 1,

  TestLabel "readInn" $ TestCase $
   assertEqual "Checks if string is legal and returns raw location data(an index number of board) for board" (6,"right")
   $ readInn "1 3 right",

  TestLabel "readStr"$ TestCase $
   assertEqual "reads 1 and last string" ("1","2")
   $  readStr "1 2",

  TestLabel "copErr" $ TestCase $
   assertEqual  "returns fatal err on COP" (False, '-') copErr,

  TestLabel "readCOP" $ TestCase $
   assertEqual "reads string if player wants computer operated opponent" (True,'O')
   $ readCOP "yes x",

  TestLabel "copGetLegalSpace"$ TestCase $
   assertEqual "returns legal space on board for COP" 8
   $copGetLegalSpace "XXXXXXXX " (0,8,1),

  TestLabel "copHandleRoll" $ TestCase $
   assertEqual"returns roll for COP" "right"
   $ copHandleRoll 1 
  ]


main :: IO ()
main = do
  doctest ["-isrc", "src/ComputerOperatedPlayer.hs", "src/GameHandler"] -- DocTest
  
  _ <- runTestTT unitTests -- UnitTest
  
  hspec $ do -- Hspec
    describe "countOcc" $ do
      it "counts occurrances of a char from string" $ do
        countOcc 'J' "Jason, Jason Bourne" `shouldBe` 2
        countOcc 'B' "BOB DYLAN IS OP" `shouldBe` 2

      context "if no char matches" $
        it "returns 0" $
          countOcc '!' "" `shouldBe` 0

    describe "createBoard" $ do
      it "creates a new board" $ do
        createBoard `shouldBe` "         "

    describe "updateBoard" $ do
      it "updates board based on index data and char" $ do
        updateBoard createBoard (0,'X') `shouldBe` "X        "
        updateBoard "O X OXX  " (0,'X') `shouldBe` "X X OXX  "

    describe "printBoard" $ do
      it "converts raw board data to printable kawaii string" $ do
        printBoard createBoard `shouldBe` " \t   1  2  3 \n#\t1 [ ][ ][ ]\n#\t2 [ ][ ][ ]\n#\t3 [ ][ ][ ]"
        printBoard "O X OXX  " `shouldBe` " \t   1  2  3 \n#\t1 [O][ ][X]\n#\t2 [ ][O][X]\n#\t3 [X][ ][ ]"

    describe "swapEarlyMarks" $ do
      it "swaps the first and third mark on board" $ do
        swapEarlyMarks createBoard `shouldBe` "         "
        swapEarlyMarks "O X OXX  " `shouldBe` "X O OXX  "

    describe "roll" $ do
      it "swaps the first and third mark on board then rolls board to a direction" $ do
        roll createBoard "left" `shouldBe` createBoard
        roll "O X OXX  " "right" `shouldBe` "X X O  XO"
        roll "O X OXX  " "left" `shouldBe`  "OX  O X X"

    describe "checkDraw" $ do
      it "checks board for draw" $ do
        checkDraw createBoard 0 `shouldBe` False
        checkDraw "OXOXOXXOX" 0 `shouldBe` True
        checkDraw "O X OXX  " 0 `shouldBe`  False

    describe "checkBoard" $ do
      it "checks board for win condition" $ do
        checkBoard createBoard  `shouldBe` (False,' ',"")
        checkBoard "OXOXOXXOX"  `shouldBe` (False,' ',"")
        checkBoard "O X OXX X"  `shouldBe` (True,'X',"O | O|X |")

    describe "readUnit" $ do
      it "reads string and checks for err " $ do
        readUnit "1"  `shouldBe` 1
        readUnit "1 12311"  `shouldBe` 1
        readUnit "1 12311a"  `shouldBe` 1

      context "if err is encountered" $ do
        it "returns 13" $ do
          readUnit ""  `shouldBe` 13 --err
          readUnit "1a"  `shouldBe` 13 --err

    describe "getPlayerTurn" $ do
      it "gets current turn and returns current- and next turn" $ do
        getPlayerTurn 'X'  `shouldBe` ('X','O')
        getPlayerTurn 'O'  `shouldBe` ('O','X')

    describe "gameLoopErrHandlr" $ do
      it "Gets raw index data for board, and checks for err" $ do
        gameLoopErrHandlr 11 `shouldBe` True
        gameLoopErrHandlr 12 `shouldBe` True
        gameLoopErrHandlr 13 `shouldBe` True
      context "if no err is encountered" $
        it "returns False" $
          gameLoopErrHandlr 1  `shouldBe` False

    describe "convertInn" $ do
      it "Converts input to data nessescary for board. and checks for err" $ do
        convertInn ["1","2"] `shouldBe` (1,2,"2")
      context "if roll is encountered" $
        it "returns with roll" $
          convertInn ["1","2", "right"]  `shouldBe` (1,2,"right")
      context "if incorrect format encountered" $
        it "returns err code" $
          convertInn ["a","a"]  `shouldBe` (13,13,"a")

    describe "genRandNum" $ do
      it "gets a random num based on min max and seed" $ do
        genRandNum 1 2 1  `shouldBe` 2
        genRandNum 1 2 10  `shouldBe` 1

    describe "readInn" $ do
      it "Checks if string is legal and returns raw location data(an index number of board) for board" $ do
        readInn "1 2" `shouldBe` (3,"")
        readInn "1 3 right"  `shouldBe`(6,"right")
      context "if too many numbers are inputted" $
        it "returns err code, and roll is not sent" $
          readInn "1 3 3 right"  `shouldBe` (11,"")
      context "if too high numbers are inputted" $
        it "returns err code, and roll is not sent" $
          readInn "9 9 right"  `shouldBe` (12,"")

    describe "readStr" $ do
      it "reads 1 and last string" $ do
        readStr "1 2" `shouldBe` ("1","2")

    describe "copErr" $ do
      it "returns fatal err on COP" $ do
        copErr `shouldBe` (False, '-')

    describe "readCOP" $ do
      it "reads string if player wants computer operated opponent" $ do
        readCOP "mongobongo" `shouldBe` copErr
      context "if no is inputted" $
        it "returns false for 1vCOP gameplay" $
          readCOP "no" `shouldBe` (False, '+')
      context "if legal yes is inputted" $
        it "returns true for 1vCOP gameplay and assigned role for COP" $
          readCOP "yes x" `shouldBe` (True,'O')
      context "if illegal yes is inputted" $
        it "returns fatal err on COP" $ do
          readCOP "yes" `shouldBe` copErr
          readCOP "yes mom" `shouldBe` copErr

    describe "copGetLegalSpace" $ do
      it "returns legal space on board for COP" $ do
        copGetLegalSpace createBoard (0,8,1) `shouldBe` 5
        copGetLegalSpace "XXXXXXXX " (0,8,1) `shouldBe` 8
      context "list has no spaces left" $
        it "returns unused err" $ do
          copGetLegalSpace "XXXXXXXXX" (0,8,1) `shouldBe` -1

    describe "copHandleRoll" $ do
      it "returns roll for COP" $ do
        copHandleRoll 1 `shouldBe` "right"
        copHandleRoll 6 `shouldBe` "left"
        copHandleRoll 2 `shouldBe` ""