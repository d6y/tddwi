
longer : Nat -> Nat -> Nat
longer x y = if x > y then x else y

lld : IO ()
lld = do
  s1 <- getLine
  s2 <- getLine
  let winner = longer (length s1) (length s2)
  putStrLn (show winner)

llb : IO ()
llb = getLine >>= \s1 => getLine >>= \s2 => putStrLn (show (longer (length s1) (length s2)))


