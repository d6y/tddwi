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


eval_guess : (target : Nat) -> (input: Nat) -> Maybe (String, Nat)
eval_guess target input =
  case compare input target of
    LT => Just ("Too low", target)
    EQ => Nothing
    GT => Just ("Too high", target)

guess : (target : Nat) -> IO ()
guess target =
  do putStrLn "I'm thinking of a number."
     replNumWith target "What's your guess? " eval_guess
