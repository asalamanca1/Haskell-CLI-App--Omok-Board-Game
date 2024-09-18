module Board where

    -- take an integer and return an empty nxn board, where n is a positive number.
    mkBoard :: Int -> [[Int]]  
    --use 'replicate' to create a list of n elements, where each element represents a row
    --each row is initialized to a list of n zeros, representing an empty row
    --returns an empty nxn board
    mkBoard n = replicate n (replicate n 0)


    --represents first player (1)
    mkPlayer :: Int
    mkPlayer = 1


    --represents second player (2)
    mkOpponent :: Int
    mkOpponent = 2


    --return the size of the board
    size :: [[Int]] -> Int
    size bd = length bd


    -- Return a column y of a board bd, where y is a 1-based index.
    row :: Int -> [[Int]] -> [Int]   
    row y bd 
        | bd == [[]] = []
        | y > size bd = [] 
        | y<1 = []
        | otherwise = bd !! (y-1)
        


    column :: Int -> [[Int]] -> [Int]
    column x bd 
        | bd == [[]] = []
        | x > length bd = [] 
        | x<1 = []
        | otherwise = map (!!(x-1)) bd


    -- diagonalRow1 :: [[Int]] -> [Int]
    -- diagonalRow1 bd
    --     | bd == [[]] = []
    --     | otherwise = [bd !! i !! i | i <- [0..size bd - 1]]

    -- diagonalRow2 :: [[Int]] -> [Int]
    -- diagonalRow2 bd
    --     | bd == [[]] = []
    --     | otherwise = [bd !! i !! (size bd - 1 - i) | i <- [0..size bd - 1]]


    mark :: Int -> Int -> [[Int]] -> Int -> [[Int]]
    mark x y bd player
        | null bd || x < 1 || x > size bd || y < 1 || y > size bd = bd
        | otherwise = take (y-1) bd ++ [markRow x (bd !! (y-1)) player] ++ drop y bd


    markRow :: Int -> [Int] -> Int -> [Int]
    markRow x row player
        | null row || x < 1 || x > length row = row
        | otherwise = take (x-1) row ++ [player] ++ drop x row


    isEmpty :: Int -> Int -> [[Int]] -> Bool
    isEmpty x y bd 
        | (row y bd) !! (x-1) == 0 = True
        | otherwise = False


    isMarked :: Int -> Int -> [[Int]] -> Bool
    isMarked x y bd 
        | (row y bd) !! (x-1) == 1 = True
        | (row y bd) !! (x-1) == 2 = True
        | otherwise = False


    isMarkedBy :: Int -> Int -> [[Int]] -> Int -> Bool
    isMarkedBy x y bd player
        | (row y bd) !! (x-1) == player = True
        | otherwise = False


    marker :: Int -> Int -> [[Int]] -> Int
    marker x y bd = (row y bd) !! (x-1)


    isFull :: [[Int]] -> Bool
    isFull bd = length (filter (\x->x == 0) (concat bd)) == 0

    checkForVerticalWin :: [Int] -> Int -> Int -> Int -> Bool
    checkForVerticalWin column player x count
        | count >= 5 = True
        | x == length column = count == 5
        | (column !! x) == player = checkForVerticalWin column player (x+1) (count+1)
        | otherwise = checkForVerticalWin column player (x+1) 0

    checkForHorizontalWin :: [Int] -> Int -> Int -> Int -> Bool
    checkForHorizontalWin row player y count
        | count == 5 = True
        | y == length row = count == 5
        | (row !! y) == player = checkForHorizontalWin row player (y+1) (count+1)
        | otherwise = checkForHorizontalWin row player (y+1) 0

    -- checkForDiagonalWin :: [Int] -> Bool
    -- checkForDiagonalWin diagonalRow player x count
    --     | count == 5 = True
    --     | x == length row = count == 5
    --     | (diagonalRow !! x) == player = checkForDiagonalWin diagonalRow player (x+1) (count+1)
    --     | otherwise = checkForDiagonalWin diagonalRow player (x+1) 0



    hasWinSeq :: [[Int]] -> Int -> Int -> Int -> Bool
    hasWinSeq bd player x y
        | y == ((size bd)+1) = False 
        | (checkForHorizontalWin (row y bd) player 0 0) == True = True
        | (checkForVerticalWin (column x bd) player 0 0) == True = True
        -- | (checkForDiagonalWin (diagonalRow1 bd) player 0 0) == True = True
        -- | (checkForDiagonalWin (diagonalRow2 bd) player 0 0) == True = True
        | x == (size bd) = hasWinSeq bd player 1 (y+1)
        | otherwise = hasWinSeq bd player (x+1) y

    isWonBy :: [[Int]] -> Int -> Bool
    isWonBy bd player
        | (player /= mkPlayer) && (player /= mkOpponent) = False
        | (bd == [[]] || length bd == 0) = False
        | (hasWinSeq bd player 1 1) == True = True
        | otherwise = False

    isGameOver :: [[Int]] -> Bool
    isGameOver bd
        | (isWonBy bd mkPlayer == True) || (isWonBy bd mkOpponent == True) || (isDraw bd == True) = True
        | otherwise = False

    isDraw :: [[Int]] -> Bool
    isDraw bd = isFull bd


    getHorizontalWin :: [Int] -> Int -> Int -> Int -> Int -> [Int] -> [Int]
    getHorizontalWin row player x y index winningRow
        | length winningRow >= 10 = winningRow
        | index == length row = winningRow
        | (row !! index) == player = getHorizontalWin row player (x+1) (y) (index + 1) (winningRow ++ [index+1,y])
        | otherwise = getHorizontalWin row player (x+1) (y) (index + 1) []

    getVerticalWin :: [Int] -> Int -> Int -> Int -> Int -> [Int] -> [Int]
    getVerticalWin column player x y index winningRow
        | length winningRow >= 10 = winningRow
        | index == length column = winningRow
        | (column !! index) == player = getVerticalWin column player (x) (y+1) (index + 1) (winningRow ++ [x,index+1])
        | otherwise = getVerticalWin column player (x) (y+1) (index + 1) []

    getWinningRow :: [[Int]] -> Int -> Int -> Int -> [Int] 
    getWinningRow bd player x y
        | length (getHorizontalWin (row y bd) player x y 0 []) >= 10 = getHorizontalWin (row y bd) player x y 0 []
        | length (getVerticalWin (column x bd) player x y 0 []) >= 10 = getVerticalWin (column x bd) player x y 0 []
        | otherwise = []

   






