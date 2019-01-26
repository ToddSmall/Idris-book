repl' : (prompt : String) -> (onInput : String -> String) -> IO ()
repl' prompt onInput = do putStr prompt
                          input <- getLine
                          putStrLn (onInput input)
                          repl' prompt onInput


replWith' : (state : a) -> (prompt  : String) -> (onInput : a -> String -> Maybe (String, a)) -> IO ()
replWith' state prompt onInput = 
  do putStr prompt
     input <- getLine
     case onInput state input of
          Nothing => pure ()
          Just (output, next_state) => do putStr output
                                          replWith' next_state prompt onInput


sumInputs : Integer -> String -> Maybe (String, Integer)
sumInputs tot inp
  = let val = cast inp in
          if val < 0
              then Nothing
              else let newVal = tot + val in
                      Just ("Subtotal: " ++ show newVal ++ "\n", newVal)