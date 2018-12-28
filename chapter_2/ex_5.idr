palindrome : Nat -> String -> Bool
palindrome min str = let strL = toLower str in
                         if length str <= min then False else strL == reverse strL