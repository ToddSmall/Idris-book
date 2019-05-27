import Data.Vect

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m = rewrite myPlusCommutes k m in rewrite plusSuccRightSucc m k in Refl

reverseProof_nil : Vect m elem -> Vect (plus m 0) elem
reverseProof_nil {m} xs = rewrite plusZeroRightNeutral m in xs

reverseProof_xs : Vect (S (m + k)) elem -> Vect (plus m (S k)) elem
reverseProof_xs {m} {k} xs = rewrite sym (plusSuccRightSucc m k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs) = reverseProof_xs (reverse' (x :: acc) xs)
