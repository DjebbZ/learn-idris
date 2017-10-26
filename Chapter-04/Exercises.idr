data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

insert : Ord elem => elem -> Tree elem -> Tree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = case (compare x val) of
                                      LT => Node (insert x left) val right
                                      EQ => orig
                                      GT => Node left val (insert x right)


{- 1.
Write a function, listToTree : Ord a => List a -> Tree a, that inserts every element of a list into a binary search tree.
You can test this at the REPL as follows:
*ex_4_1> listToTree [1,4,3,5,2]
Node (Node Empty 1 Empty)
     2
     (Node (Node Empty 3 (Node Empty 4 Empty))
           5
           Empty) : Tree Integer
-}

listToTree : Ord a => List a -> Tree a
listToTree [] = Empty
listToTree (x :: xs) = insert x (listToTree xs)


{- 2.
Write a corresponding function, treeToList : Tree a -> List a, that flattens a tree into a list using in-order traversal
(that is, all the values in the left subtree of a node should be added to the list before the value at the node, which should be added before the values in the right subtree).
If you have the correct answers to exercises 1 and 2, you should be able to run this:
*ex_4_1> treeToList (listToTree [4,1,8,7,2,3,9,5,6])

[1, 2, 3, 4, 5, 6, 7, 8, 9] : List Intege
-}

treeToList : Tree a -> List a
treeToList Empty = []
treeToList (Node left val right) = treeToList left ++ val :: treeToList right

{- 3.
An integer arithmetic expression can take one of the following forms:
A single integer
Addition of an expression to an expression
Subtraction of an expression from an expression
Multiplication of an expression with an expression
Define a recursive data type, Expr, that can be used to represent such expressions.
Hint: Look at the Picture data type and see how the informal description mapped to the data declaration.
-}

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr

{- 4.
Write a function, evaluate : Expr -> Int, that evaluates an integer arithmetic expression.
If you have correct answers to 3 and 4, you should be able to try something like the following at the REPL:
*ex_4_1> evaluate (Mult (Val 10) (Add (Val 6) (Val 3)))
90 : Int
-}

evaluate : Expr -> Int
evaluate (Val x) = x
evaluate (Add x y) = (+) (evaluate x) (evaluate y)
evaluate (Sub x y) = (-) (evaluate x) (evaluate y)
evaluate (Mult x y) = (*) (evaluate x) (evaluate y)

{- 5.
Write a function, maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a,
that returns the larger of the two inputs, or Nothing if both inputs are Nothing.
For example:
*ex_4_1> maxMaybe (Just 4) (Just 5)
Just 5 : Maybe Integer

*ex_4_1> maxMaybe (Just 4) Nothing
Just 4 : Maybe Integer
-}

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe (Just x) Nothing = Just x
maxMaybe Nothing (Just y) = Just y
maxMaybe (Just x) (Just y) = Just (max x y)

{- 6.
Write a function, biggestTriangle : Picture -> Maybe Double, that returns the area of the biggest triangle in a picture, or Nothing if there are no triangles. For example, you can define the following pictures:
testPic1 : Picture
testPic1 = Combine (Primitive (Triangle 2 3))
                   (Primitive (Triangle 2 4))

testPic2 : Picture
testPic2 = Combine (Primitive (Rectangle 1 3))
                   (Primitive (Circle 4))
Then, test biggestTriangle at the REPL as follows:
*ex_4_1> biggestTriangle testPic1
Just 4.0 : Maybe Double

*ex_4_1> biggestTriangle testPic2
Nothing : Maybe Double
-}
data Shape = ||| A triangle, base and height
             Triangle Double Double
           | ||| width and length
             Rectangle Double Double
           | ||| radius
             Circle Double

area : Shape -> Double
area (Triangle base height) = 0.5 * base * height
area (Rectangle width height) = width * height
area (Circle radius) = pi * radius * radius



data Picture = Primitive Shape
             | Combine Picture Picture
             | Rotate Double Picture
             | Translate Double Double Picture

rectangle : Picture
rectangle = Primitive (Rectangle 20 10)

circle : Picture
circle = Primitive (Circle 5)

triangle : Picture
triangle = Primitive (Triangle 10 10)


||| a function that returns the area of the largest triangle in a Picture
biggestTriangle : Picture -> Maybe Double
biggestTriangle (Primitive tri@(Triangle x y)) = Just (area tri)
biggestTriangle (Primitive (Rectangle l h)) = Nothing
biggestTriangle (Primitive (Circle r)) = Nothing
biggestTriangle (Combine pic1 pic2) = maxMaybe (biggestTriangle pic1) (biggestTriangle pic2)
biggestTriangle (Rotate angle pic) = biggestTriangle pic
biggestTriangle (Translate x y pic) = biggestTriangle pic
