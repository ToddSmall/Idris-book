data Vect : Nat -> Type -> Type where
     Nil  : Vect Z a
     (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

%name Vect xs, ys, zs

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
    foldr func acc [] = acc
    foldr func acc (x :: xs) = func x (foldr func acc xs)
