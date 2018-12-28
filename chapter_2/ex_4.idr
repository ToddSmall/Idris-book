palindrome : String -> Bool
palindrome str = let strL = toLower str in
                     if length strL <= 10 then False else strL == reverse strL
