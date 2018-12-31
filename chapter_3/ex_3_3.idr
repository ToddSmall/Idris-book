import Data.Vect

createEmpties : Vect n (Vect 0 elem)
createEmpties {n = Z} = []
createEmpties {n = (S k)} = [] :: createEmpties

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in 
                         zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

dotProd : Num a => Vect m a -> Vect m a -> a
dotProd v1 v2 = sum (zipWith (*) v1 v2)

multVectMatrix : Num a => Vect m a -> Vect n (Vect m a) -> Vect n a
multVectMatrix v mat = map (\v2 => dotProd v v2) mat

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] m2 = []
multMatrix (x :: xs) m2 = multVectMatrix x (transposeMat m2) :: multMatrix xs m2
