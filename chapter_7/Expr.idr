data Expr num = Val num
              | Add (Expr num) (Expr num)
              | Sub (Expr num) (Expr num)
              | Mul (Expr num) (Expr num)
              | Div (Expr num) (Expr num)
              | Mod (Expr num) (Expr num)
              | Abs (Expr num)

eval : (Abs num, Neg num, Integral num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Mod x y) = eval x `mod` eval y
eval (Abs x) = abs (eval x)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

(Num ty, Integral ty) => Integral (Expr ty) where
    div = Div
    mod = Mod

Neg ty => Neg (Expr ty) where
    negate x = 0 - x
    (-) = Sub

Abs ty => Abs (Expr ty) where
    abs = Abs

Show ty => Show (Expr ty) where
    show (Val x) = show x
    show (Add x y) = "(" ++ show x ++ " + " ++ show y ++ ")"
    show (Sub x y) = "(" ++ show x ++ " - " ++ show y ++ ")"
    show (Mul x y) = "(" ++ show x ++ " * " ++ show y ++ ")"
    show (Div x y) = "(" ++ show x ++ " / " ++ show y ++ ")"
    show (Abs x) = "abs(" ++ show x ++ ")"

(Abs ty, Eq ty, Integral ty, Neg ty) => Eq (Expr ty) where
    (==) x y = eval x == eval y

(Abs ty, Integral ty, Neg ty) => Cast (Expr ty) ty where
    cast x = eval x

Functor Expr where
    map func (Val x) = Val (func x)
    map func (Add x y) = Add (map func x) (map func y)
    map func (Sub x y) = Sub (map func x) (map func y)
    map func (Mul x y) = Mul (map func x) (map func y)
    map func (Div x y) = Div (map func x) (map func y)
    map func (Mod x y) = Mod (map func x) (map func y)
    map func (Abs x) = Abs (map func x)
    