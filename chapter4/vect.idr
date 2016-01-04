import Data.Fin

data Vect : Nat -> Type -> Type where
  Nil : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

append : Vect n elem -> Vect m elem -> Vect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

v1 : Vect 3 Int
v1 = 1 :: 2 :: 3 :: Nil

v2 : Vect 2 Int
v2 = 9 :: 8 :: Nil

{-
append v1 v2
[1, 2, 3, 9, 8] : Vect 5 Int
-}

vs : Vect 3 String
vs = ["One", "Two", "Three"]

zip : Vect n a -> Vect n b -> Vect n (a,b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

{-
zip v1 vs
[(1, "One"), (2, "Two"), (3, "Three")] : Vect 3 (Int, String)
-}

{-
Ex. 3 and 4, section 4.2.4
-}
take : (t : Fin (S n)) -> Vect n elem -> Vect (finToNat t) elem
take FZ xs = []
take (FS n) (y :: ys) = y :: take n ys

{- Ex 5
pos is indexed from zero into the vector

sumEntries 2 [1,2,3,4] [5,6,7,8]
Just 10 : Maybe Integer

-}

index : (Fin n) -> Vect n elem -> elem
index FZ (x :: xs) = x
index (FS n) (x :: xs) = index n xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos xs ys = case integerToFin pos n of
                            Nothing  => Nothing
                            (Just f) => Just ((index f xs) + (index f ys))



