import Data.Vect

fourInts : Vect 4 Nat
fourInts = [0, 1, 2, 3]

sixInts : Vect 6 Nat
sixInts = [4, 5, 6, 7, 8, 9]

tenInts : Vect 10 Nat
tenInts = fourInts ++ sixInts
