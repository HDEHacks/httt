data Piece = Cross | Naught deriving (Show, Eq)
type Board = [Maybe Piece]

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
stringBoard b = format (map only b)
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

main :: IO ()
main = return ()
