data Piece = Cross | Naught deriving (Show, Eq)
type Board = [(Maybe Piece)]

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

-- winner :: Board -> Bool
-- winner b = let possibilities = zones b
--                filled = filter (\x -> not (elem Nothing x)) possibilities in
--                any (\x -> all (== (head x) x)) filled--(all (== (head filled) ) filled)