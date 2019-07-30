data Piece = Cross | Naught deriving (Show, Eq)
type Board = [Maybe Piece]

-- Board logic: this is monadic
emptyBoard :: Board
emptyBoard = take 9 $ repeat Nothing

putPiece :: Board -> Piece -> Int -> Maybe Board
putPiece board piece n = if not (0 <= n && n < 9) then Nothing
    else case (board !! n) of 
        Just _ -> Nothing
        _ -> Just $ (take n board) ++ [Just piece] ++ (drop (n + 1) board)

zones :: Board -> [[Maybe Piece]]
zones [a,b,c,
       d,e,f,
       g,h,i] = [[a,b,c], [d,e,f], [g,h,i],
                 [a,d,g], [b,e,h], [c,f,i],
                 [a,e,i], [g,e,c]]

winner :: Board -> Maybe Piece
winner b = let winners = filter (\z -> all (== (head z)) z) (zones b)
           in if length winners == 0 then Nothing else head . head $ winners

-- Display logic
stringBoard :: Board -> String
stringBoard board = format (map only board)
       where only :: Maybe Piece -> String
             only (Just Cross) = "X"
             only (Just Naught) = "O"
             only _ = " "
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
readMaybe :: String -> Maybe Int
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

announce :: Board -> Piece -> IO ()
announce board winner = do printBoard board
                           putStrLn $ (show winner) ++ " wins!"

tryMove :: Board -> Piece -> Int -> IO ()
tryMove board piece position = case putPiece board piece position of
    Just next -> maybe (play next) (announce next) (winner next)
    _ -> do putStrLn "You can't play there." ; play board

getInt :: IO Int
getInt = do line <- getChar
            putStrLn ""
            let parse = readMaybe [line]
            let recurse = do putStrLn "That's not a number." ; getInt
            maybe recurse pure parse

play :: Board -> IO ()
play board = let turn       = length $ filter (/= Nothing) board
                 isXTurn    = turn `mod` 2 == 0
                 whoseTurn  = if isXTurn then Cross else Naught 
             in if turn >= 9 then putStrLn "It's a tie!"
                else do printBoard board
                        putStrLn $ "It's " ++ (show whoseTurn) ++ "'s move."
                        position <- getInt
                        tryMove board whoseTurn position

main :: IO ()
main = play emptyBoard
