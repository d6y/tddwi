import Data.Vect

read_vect : IO (len ** Vect len String)
read_vect = do x <- getLine
               if (x == "")
                  then pure (_ ** [])
                  else do (_ ** xs) <- read_vect
                          pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank line to end):"
               (len1 ** vec1) <- read_vect
               putStrLn "Enter second vector (blank line to end):"
               (len2 ** vec2) <- read_vect
               case exactLength len1 vec2 of
                    Nothing => putStrLn "Vectors are different lengths"
                    Just vec2' => printLn (zip vec1 vec2')

readToBlank : IO (List String)
readToBlank =
  do printLn "Enter a list one line at a time, blank line when done"
     item <- getLine
     if item == ""
        then pure []
        else do items <- readToBlank
                pure (item :: items)

r2b : IO (List String)
r2b = getLine >>= process
  where
    process : String -> IO (List String)
    process "" = pure []
    process s  = map (s ::) r2b

test : IO ()
test = do xs <- r2b
          printLn xs

readAndSave : IO ()
readAndSave =
  do putStrLn "Enter list (blank line when done)"
     xs <- r2b
     putStrLn "Enter filename to save to"
     filename <- getLine
     result <- writeFile filename (show xs)
     case result of
      (Left err) => printLn err
      (Right r)  => putStrLn "Saved"





