over_length : Nat -> List String -> Nat
over_length min xs = length (filter (\x => length x > min) xs)