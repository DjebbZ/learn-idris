||| Represents shapes
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
