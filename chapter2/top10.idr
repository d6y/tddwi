module Main

{-
7. Write a function top_ten, of type Ord a => List a -> List a which returns the ten
largest values in a list. You may find the following Prelude functions useful:
  1. take : Nat -> List a -> List a
  2. sort : Ord a => List a -> List a
-}
top_ten : Ord a => List a -> List a
top_ten xs = take 10 (reverse (sort xs))

{-
8. Write a function over_length, of type Nat -> List String -> Nat
which returns the number of strings in the list longer than the given number of characters.
For example, evaluating over_length 3 ["One", "Two", "Three", Four"] should give the output 2.
-}
over_length : Nat -> List String -> Nat
over_length n xs = length (filter (longStrings) (map length xs))
   where
     longStrings = (> n)

main : IO ()
main = printLn (over_length 3 ["One", "Two", "Three", "Four"])
-- main = printLn (top_ten [1, 2, 99, 1023, -222, 3, 4, 5, 9, 10, 100, 0, 123456789])
