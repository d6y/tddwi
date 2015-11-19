module Main

{-
6. Write a function counts, of type String -> (Nat, Nat),
   which returns a pair of the number of words in the input
   and the number of characters in the input.
   For example, the input "Hello, Idris world!" should give the output (3, 19).
-}
counts : String -> (Nat, Nat)
counts s = ( length (words s), length s )

showCounts : String -> String
showCounts s = let cs = counts s
                   wc = fst cs
                   cc = snd cs in
                   "Words: " ++ (show wc) ++ "\n" ++
                   "Chars: " ++ (show cc) ++ "\n"

main : IO ()
main = repl "Enter string: " showCounts
