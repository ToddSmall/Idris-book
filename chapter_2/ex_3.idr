palindrome : String -> Bool
palindrome str = let strL = toLower str in
                     strL == reverse strL
