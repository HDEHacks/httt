data Piece = Cross | Naught deriving (Show, Eq)
type Board = [Maybe Piece]
data GameOver = Full | Piece deriving (Show)

-- Board logic: this is monadic
emptyBoard :: Board
emptyBoard = take 9 $ repeat Nothing

putPiece :: Board -> Piece -> Int -> Maybe Board
putPiece board piece n = if not (0 <= n && n < 9) then Nothing
    else case (board !! n) of 
        Just _ -> Nothing
        Nothing -> Just $ (take n board) ++ [Just piece] ++ (drop (n + 1) board)

zones :: Board -> [[Maybe Piece]]
zones [a,b,c,
       d,e,f,
       g,h,i] = [[a,b,c], [d,e,f], [g,h,i],
                 [a,d,g], [b,e,h], [c,f,i],
                 [a,e,i], [g,e,c]]

winner :: Board -> Maybe Piece
winner b = head . head $ filter (\z -> all (== (head z)) z) (zones b)

-- Display logic
stringBoard :: Board -> String
stringBoard board = format (map only board)
       where only :: Maybe Piece -> String
             only Nothing = " "
             only (Just Cross) = "X"
             only (Just Naught) = "O"
             format :: [String] -> String
             format [a,b,c,
                     d,e,f,
                     g,h,i] = a ++ " | " ++ b ++ " | " ++ c ++ "\n" ++
                             "---------\n" ++
                              d ++ " | " ++ e ++ " | " ++ f ++ "\n" ++
                             "---------\n" ++
                              g ++ " | " ++ h ++ " | " ++ i ++ "\n"

printBoard :: Board -> IO ()
printBoard = putStrLn . stringBoard

-- Play Logic
whichTurn :: Bool -> String
whichTurn True = "It's X's move."
whichTurn False = "It's O's move."

readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

tryMove :: Board -> Piece -> Int -> IO ()
tryMove board piece position = case putPiece board piece position of
    Nothing -> do putStrLn "You can't play there."
                  play board
    Just newBoard -> case winner newBoard of
                        Nothing -> play newBoard
                        Just b -> let w = show b in
                                  do printBoard newBoard
                                     putStrLn $ w ++ " wins!"

play :: Board -> IO ()
play board = let turn       = length $ filter (/= Nothing) board
                 isCatsGame = turn >= 9
                 isXTurn    = turn `mod` 2 == 0
                 whoseTurn  = if isXTurn then Cross else Naught
             in if isCatsGame then (putStrLn "It's a tie!")
                else do printBoard board
                        putStrLn $ whichTurn isXTurn
                        line <- getChar
                        putStrLn ""
                        case readMaybe [line] of
                          Nothing -> do putStrLn "You can't play there."
                                        play board
                          Just position -> tryMove board whoseTurn position

main :: IO ()
main = play emptyBoard
