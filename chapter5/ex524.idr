import System

readNumber : IO (Maybe Nat)
readNumber = do
 input <- getLine
 if all isDigit (unpack input)
    then pure (Just (cast input))
    else pure Nothing

replNumWith : (state   :  a) ->
              (prompt  : String) ->
              (onInput : (a -> Nat -> Maybe (String,a))) ->
              IO ()
replNumWith state prompt onInput =
  do putStr prompt
     Just n <- readNumber | Nothing => punish
     case onInput state n of
        Nothing          => pure ()
        (Just (msg, s')) => do putStrLn msg
                               replNumWith s' prompt onInput
  where
    punish : IO ()
    punish = do putStrLn "Please enter number"
                replNumWith state prompt onInput

data State = Progress Nat Nat

eval_guess : (state: State) -> (input: Nat) -> Maybe (String, State)
eval_guess (Progress target guesses) input =
  case compare input target of
    LT => Just ( (show guesses) ++ " - too low", Progress target (1 + guesses) )
    EQ => Nothing
    GT => Just ( (show guesses) ++ " - too high", Progress target (1 + guesses) )

guess : (target : Nat) -> IO ()
guess target =
  do putStrLn "I'm thinking of a number."
     replNumWith (Progress target 1) "What's your guess? " eval_guess


main : IO ()
main = randomNat >>= guess
  where
    randomNat : IO Nat
    randomNat = do random <- time
                   let upto100 = mod random 100
                   pure (fromIntegerNat upto100)
