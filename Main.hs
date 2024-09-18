module Main where

import Board
import System.IO

main :: IO ()
main = do 
  coordinates <- play (mkBoard 15) mkPlayer
  putStrLn "Winning row coordinates:"
  print coordinates
  putStrLn "GAME OVER, THANKS FOR PLAYING BIDUR >:D"


printBoard :: [[Int]] -> IO ()
printBoard board = do
  -- Print the column indices, showing only the last digit
  let indexes = concatMap (\x -> show (x `mod` 10) ++ " ") [1..length (head board)]
  putStrLn $ "  x " ++ indexes
  putStrLn "y |------------------------------"  -- Adjust the number of dashes if needed
  -- Print each row with the row number (last digit only)
  let rows = zip [1..] board  -- Pair each row with its row number
  mapM_ printRow rows



-- Helper function to print each row
printRow :: (Int, [Int]) -> IO ()
printRow (rowNum, row) = do
  let line = concatMap (\x -> [playerToChar x] ++ " ") row
  putStrLn $ show (rowNum `mod` 10) ++ " | " ++ line



playerToChar :: Int -> Char
playerToChar p
  | p == 1 = 'O'
  | p == 2 = 'X'
  | p ==3 = '@'
  | otherwise = '.'


play :: [[Int]] -> Int -> IO [Int]
play bd player = do
  putStrLn ("Player "++ show player ++"'s turn")
  --prompt user to enter coordinates
  putStrLn "Enter coordinates as two space-separated values (e.g., '3 4'):"
  line <- getLine
  --parse x coordinate from user input
  let parsedX = reads line :: [(Int, String)]
  --if user's x coordinate is null, print invalid input error message
  if null parsedX 
    then invalidInput'
  --if user's x coordinate is the correct format
  else 
    let (x, unparsedY) = head parsedX 
    --check that x coordinate is in range of board coordinates
    in if (x > 0) && (x < ((size bd)+1)) 
      then 
        let parsedY = reads unparsedY :: [(Int, String)] 
        in if null parsedY
          then invalidInput'
          else 
            --set y = the first Int element of the parsedY element
            let (y, _) = head parsedY 
            --check that y coordinate is in range of board coordinates
            in if (y > 0) && (y < ((size bd)+1)) 
              then 
                --check that the x/y coordinate on the board is empty
                if isEmpty x y bd
                  then do
                    --mark stone on board and print board
                    let newBd = mark x y bd player 
                    
                    
                    --if game is over, end game
                    if (isGameOver newBd)==True
                      then do
                        -- return [x, y]
                        let winningRow = (getWinningRow newBd player x y)
                        
                        -- printBoard (placeWinningStones newBd winningRow player)
                        -- return winningRow
                        finalBd <- placeWinningStones newBd winningRow player  -- Capture the updated board
                        printBoard finalBd  -- Print the updated board
                        putStrLn ("Winner: Player " ++ show player)
                        return winningRow
                       
                    else do
                      printBoard newBd
                      --if current turn is mkPlayer, make recursive call with updated board and mkOpponent as player turn
                      if player == mkPlayer 
                        then do
                          play newBd mkOpponent
                      --if current turn is mkOpponent, make recursive call with updated board and mkPlayer as player turn
                      else do
                        play newBd mkPlayer

                --print not empty error message if the x/y coordinates are not empty
                else do notEmpty'
              --print index out of range error message if y coordinate is out of range
              else indexOutOfRange'
      --print index out of range error message if x coordinate is out of range
      else indexOutOfRange'
  where
    invalidInput' = do
      putStrLn "Invalid input! Please enter exactly two positive integers separated by a single space."
      play bd player
    notEmpty' = do
      putStrLn "This slot on the board is not empty. Please re-enter a new set of coordinates."
      play bd player
    indexOutOfRange' = do
      putStrLn "These coordinates are out of range. Please re-enter a new set of coordinates."
      play bd player

placeWinningStones :: [[Int]] -> [Int] -> Int -> IO [[Int]]
placeWinningStones bd winningRow player = do
  let bd1 = mark (winningRow !! 0) (winningRow !! 1) bd 3
  let bd2 = mark (winningRow !! 2) (winningRow !! 3) bd1 3
  let bd3 = mark (winningRow !! 4) (winningRow !! 5) bd2 3
  let bd4 = mark (winningRow !! 6) (winningRow !! 7) bd3 3
  let bd5 = mark (winningRow !! 8) (winningRow !! 9) bd4 3
  return bd5  -- Return the final updated board