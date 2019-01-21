readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (cast input))
     else pure Nothing

readPair : IO (String, String)
readPair = do str1 <- getLine
              str2 <- getLine
              pure (str1, str2)

readNumbers_v0 : IO (Maybe (Nat, Nat))
readNumbers_v0 =
  do Just num1_ok <- readNumber
     Just num2_ok <- readNumber
     pure (Just (num1_ok, num2_ok))
     
readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do Just num1_ok <- readNumber | Nothing => pure Nothing
     Just num2_ok <- readNumber | Nothing => pure Nothing
     pure (Just (num1_ok, num2_ok))