import Data.Vect


data PowerSource = Petrol | Pedal | Electricity

data Vehicle : PowerSource -> Type where
  Bicycle : Vehicle Pedal
  Unicycle : Vehicle Pedal
  Car : (fuel : Nat) -> Vehicle Petrol
  Bus : (fuel : Nat) -> Vehicle Petrol
  Motorcycle : (fuel : Nat) -> Vehicle Petrol
  Tramway : Vehicle Electricity


wheels : Vehicle power -> Nat
wheels Bicycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Unicycle = 1
wheels (Motorcycle fuel) = 1
wheels Tramway = 100

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Bus 200
refuel (Motorcycle fuel) = Motorcycle 40
refuel Bicycle impossible
refuel Unicycle impossible
refuel Tramway impossible


{- 1.
Extend the Vehicle data type so that it supports unicycles and motorcycles, and update wheels and refuel accordingly.
-}

{- 2.
Extend the PowerSource and Vehicle data types to support electric vehicles (such as trams or electric cars).
-}

{- 3.
The take function, on List, has type Nat -> List a -> List a.
What’s an appropriate type for the corresponding vectTake function on Vect?
Hint: How do the lengths of the input and output relate?
It shouldn’t be valid to take more elements than there are in the Vect.
Also, remember that you can have any expression in a type.
-}

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake Z xs = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

{- 4.
Write a sumEntries function with the following type:
sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
It should return the sum of the entries at position pos in each of the inputs if pos is within bounds, or Nothing otherwise.
For example:
*ex_4_2> sumEntries 2 [1,2,3,4] [5,6,7,8]
Just 10 : Maybe Integer

*ex_4_2> sumEntries 4 [1,2,3,4] [5,6,7,8]
Nothing : Maybe Integer
Hint: You’ll need to call integerToFin, but you’ll only need to do it once.
-}

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries {n} pos vec1 vec2 = case (integerToFin pos n) of
                                          Nothing => Nothing
                                          (Just idx) => Just ((index idx vec1) + (index idx vec2))
