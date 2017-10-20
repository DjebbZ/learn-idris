import Data.Vect

{- 1.
Reimplement transposeMat using zipWith instead of transposeHelper.
You can test your answer at the REPL as follows:
*ex_3_3_3> transposeMat [[1,2], [3,4], [5,6]]
[[1, 3, 5], [2, 4, 6]] : Vect 2 (Vect 3 Integer)
-}

createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
                             zipWith (::) x xsTrans

{- 2.
Implement addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a).
You can test your answer at the REPL as follows:
*ex_3_3_3> addMatrix [[1,2], [3,4]] [[5,6], [7,8]]
[[6, 8], [10, 12]] : Vect 2 (Vect 2 Integer)
-}

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

{- 3.
Implement a function for multiplying matrices, following the description given in section 3.3.1. Hint: This definition is quite tricky and involves multiple steps. Consider the following:
You have a left matrix of dimensions n × m, and a right matrix of dimensions m × p. A good start is to use transposeMat on the right matrix.
Remember that you can use Ctrl-Alt-L to lift holes to top-level functions.
Remember to pay close attention to the types of the local variables and the types of the holes.
Remember to use Ctrl-Alt-S to search for expressions, and pay close attention to the types of any resulting holes.
You can test your answer at the REPL as follows:
*ex_3_3_3> multMatrix [[1,2], [3,4], [5,6]] [[7,8,9,10], [11,12,13,14]]
[[29, 32, 35, 38],
 [65, 72, 79, 86],
 [101, 112, 123, 134]] : Vect 3 (Vect 4 Integer)
-}

oneResult : Num a => Vect n a -> Vect n a -> a
oneResult [] [] = 0
oneResult (x :: xs) (y :: ys) = x * y + oneResult xs ys


oneLine : Num a => Vect cols a -> Vect rows (Vect cols a) -> Vect rows a
oneLine [] [] = []
oneLine line1 (line2 :: rest) = oneResult line1 line2 :: oneLine line1 rest


helper : Num a => (left : Vect rows1 (Vect cols1 a)) -> (right_transp : Vect cols2 (Vect cols1 a)) -> Vect rows1 (Vect cols2 a)
helper [] right_transp = []
helper (x :: xs) right_transp =
  oneLine x right_transp :: helper xs right_transp

multMatrix : Num a => Vect rows1 (Vect cols1 a) -> Vect cols1 (Vect cols2 a) -> Vect rows1 (Vect cols2 a)
multMatrix [] [] = []
multMatrix left right =
  let
    right_transp = transposeMat right
  in
    helper left right_transp
