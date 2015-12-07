import Data.Vect

example : Vect 3 (Vect 4 Integer)
example = [ [1, 2, 3, 4],
            [5, 6, 7, 8],
            [9,10,11,12] ]

addLeft : Vect 3 (Vect 2 Integer)
addLeft = [ [1, 2],
            [3, 4],
            [5, 6] ]

addRight : Vect 3 (Vect 2 Integer)
addRight = [ [7,   8],
             [9,  10],
             [11, 12] ]

multRight : Vect 2 (Vect 4 Integer)
multRight = [ [7,   8,  9, 10],
             [11, 12, 13, 14] ]

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = sum x y :: addMatrix xs ys
                                where
                                  sum xs ys = zipWith (+) xs ys

multMatrix : Num numType =>
             Vect n (Vect m numType) ->
             Vect m (Vect p nunType) ->
             Vect n (Vect p nunType)



create_empties : Vect n (Vect 0 elem)
create_empties = replicate _ []

transpose_helper : (x : Vect n elem) ->
                   (xs_trans : Vect n (Vect k elem)) ->
                   Vect n (Vect (S k) elem)
transpose_helper [] [] = []
transpose_helper (x :: xs) (y :: ys) = (x :: y) :: transpose_helper xs ys

transpose_mat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          zipWith (::) x xs_trans

{-
transpose_mat [] = create_empties
transpose_mat (x :: xs) = let xs_trans = transpose_mat xs in
                          transpose_helper x xs_trans
-}


