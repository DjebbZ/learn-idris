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


data Biggest = NoTriangle | Size Double

||| a function that returns the area of the largest triangle in a Picture
biggestTriangle : Picture -> Biggest
biggestTriangle (Primitive (Triangle x y)) = Size (area (Triangle x y))
biggestTriangle (Primitive (Rectangle l h)) = NoTriangle
biggestTriangle (Primitive (Circle r)) = NoTriangle
biggestTriangle (Combine x y) = ?combine --Size (max (biggestTriangle x) (biggestTriangle y))
biggestTriangle (Rotate x y) = biggestTriangle y
biggestTriangle (Translate x y z) = biggestTriangle z
