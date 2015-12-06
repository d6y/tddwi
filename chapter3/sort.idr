import Data.Vect

insert : Ord elem =>
        (x : elem) -> (sorted : Vect k elem) -> Vect (S k) elem
insert x [] = [x]
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

{-
insert x [] = [x]
insert x (y :: xs) = if x < y then x :: y :: xs
                              else y :: insert x xs
-}

ins_sort : Ord elem => Vect n elem -> Vect n elem
ins_sort [] = []
ins_sort (x :: xs) = let xs_sorted = ins_sort xs in
                         insert x xs_sorted
