module Main

palindrome : String -> Bool
palindrome s = let lower = toLower s in
                   lower == reverse lower


longPal : Nat -> String -> Bool
longPal n s = (length s) > n && (palindrome s)

showPalindrome : (String -> Bool) -> String -> String
showPalindrome f s = s ++ " is a palindome: " ++ show (f s) ++ "\n"

main : IO ()
main = repl "Enter string: " (showPalindrome (longPal 6))
