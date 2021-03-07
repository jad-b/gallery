module EightQueens where
    -- "Find" the queen in column k
    -- Compare against all other queens
        -- If rows or columns match or are diagonal == No go

{- The Eight Queens problem asks what the valid placements are for eight queens
 - on an 8 x 8 chess board.
 -}
{- This is the outline of a solution from SICP Ex. 2.42

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

-}
{-
queens boardSize = queenCols boardSize
    where
        -- Checks the queen in column k against the other queens.
        isSafe _ pos =
            let front = head pos

        -- adjoinPosition :: Int a, Int b => a -> b -> [[(a,a)]]
        -- Add a queen in each column of the new row
        adjoinPosition newRow k restOfQueens = map (\qs -> (newRow, k):qs) restOfQueens
        queenCols 0 = [[]]
        queenCols k =
            -- ([(a,a)] -> bool) -> [[(a,a)]] -> [[(b,b)]]
            filter -- Filter for only valid queens
                (\positions -> isSafe k positions)
                -- Prepend a new queen position option to all the existing
                -- solutions.
                (foldl -- Given a list of (row,col) tuples, prepend one more at (newRow, k)
                    -- [[(r_k,c_k),...,(r_1, c_1)]]
                    (\restOfQueens -> map (\newRow -> adjoinPosition newRow k restOfQueens) [1..boardSize])
                    queenCols (k-1))
-}
-- isSafe :: Int a => a -> [(a0,a1)] -> bool
-- isSafe k queens =
-- Two points are diagonal in a 2-d space if their difference is equal in both
-- dimensions.
isDiagonal :: (Num a, Eq a) => (a, a) -> (a, a) -> Bool
isDiagonal (a, b) (c, d) = abs (a - c) == abs (b - d)
