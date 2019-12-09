import Data.Vect

oneInVector : Elem 1 [1, 2, 3]
oneInVector = Here

maryInVector : Elem "Mary" ["Peter", "Paul", "Mary"]
maryInVector = There (There Here)
