import Data.Vect

fourInts : Vect 4 Int
fourInts = [0,1,2,3]

sixInts : Vect 6 Int
sixInts = [0,1,2,3,4,6]

tenInts : Vect 10 Int
tenInts = fourInts ++ sixInts

word_lengths : Vect len String -> Vect len Nat
word_lengths [] = []
word_lengths (x :: xs) = length x :: word_lengths xs



-- Albuquerque