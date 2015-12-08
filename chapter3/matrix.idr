import Data.Vect

-- Example data to use:

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

-- Exercises:

addMatrix : Num numType =>
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType) ->
            Vect rows (Vect cols numType)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = sum x y :: addMatrix xs ys
                                where
                                  sum xs ys = zipWith (+) xs ys


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

products : Num numType =>
  (xs : Vect m numType) ->
  (ys : Vect p (Vect m numType)) -> Vect p numType
products xs [] = []
products xs (y :: ys) = let row = zipWith (*) xs y
                            product = foldl (+) 0 row in
                            product :: products xs ys

multTM : Num numType =>
  (xs    : Vect n (Vect m numType)) ->
  (ys_tp : Vect p (Vect m numType)) -> Vect n (Vect p numType)
multTM [] _ = []
multTM (x :: xs) ys = (products x ys) :: (multTM xs ys)


multMatrix : Num numType =>
             Vect n (Vect m numType) ->
             Vect m (Vect p numType) ->
             Vect n (Vect p numType)
multMatrix xs ys = let ys_tp = transpose_mat ys in
                    multTM xs ys_tp

{-
Idris> :l matrix.idr
Type checking ./matrix.idr
*matrix> multMatrix addLeft multRight
[[29, 32, 35, 38],
 [65, 72, 79, 86],
 [101, 112, 123, 134]] : Vect 3 (Vect 4 Integer)
-}
