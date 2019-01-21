import System

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do
    putStr ("(" ++ (show guesses) ++ ") " ++ "Enter a guess between 1 and 100: ")
    Just value <- readNumber | Nothing => do putStrLn "Invalid input"
                                             guess target guesses
    case compare value target of
         LT => do putStrLn ((show value) ++ " is less than the target")
                  guess target (guesses + 1)
         EQ => putStrLn ((show value) ++ " is the target!")
         GT => do putStrLn ((show value) ++ " is greater than the target")
                  guess target (guesses + 1)


main : IO ()
main = do
    target <- time
    guess (cast (mod target 100) + 1) 0