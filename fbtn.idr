{- Idris -}
name : Bool -> String
name False = "NO"
name True = "Sure"

NameOrNumber : (b: Bool) -> Type
NameOrNumber False = Int
NameOrNumber True = String


my_tail : List a -> List a
my_tail [] = ?hole
my_tail (x :: xs) = xs


