import Data.Vect

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if (x == "")
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do lines <- readToBlank
                 putStr "Enter a filename: "
                 filepath <- getLine
                 Right () <- writeFile filepath (unlines lines) | Left err => putStrLn (show err)
                 pure ()

fGetLines : (h : File) -> IO (Either FileError (List String))
fGetLines h = do at_eof <- fEOF h
                 case at_eof of
                    False => do Right x <- fGetLine h | Left err => pure (Left err)
                                Right xs <- fGetLines h | Left err => pure (Left err)
                                pure (Right (x :: xs))
                    True => pure (Right [])

fEchoLines : IO (Either FileError (List String))
fEchoLines = do putStr "Enter filename: "
                filename <- getLine
                Right h <- openFile filename Read | Left err => pure (Left err)
                lines <- fGetLines h | Left err => pure (Left err)
                closeFile h
                pure lines

readVectFile : (filename: String) -> IO (n ** Vect n String)
readVectFile filename = do Right h <- openFile filename Read | Left err => pure (_ ** [])
                           Right lines <- fGetLines h | Left err => pure (_ ** [])
                           closeFile h
                           pure (_ ** (fromList lines))
