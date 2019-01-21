printLonger : IO ()
printLonger = do putStr "First string: "
                 first_string <- getLine
                 putStr "Second string: "
                 second_string <- getLine
                 putStrLn (show (max (length first_string) (length second_string)))

printLonger2 : IO ()
printLonger2 = putStr "First string: " >>= \_ =>
               getLine >>= \input1 =>
               putStr "Second string: " >>= \_ =>
               getLine >>= \input2 =>
               putStrLn (show (max (length input1) (length input2)))
