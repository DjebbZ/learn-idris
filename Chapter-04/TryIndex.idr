import Data.Vect

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex x [] = Nothing
tryIndex {n} x xs = case (integerToFin x n) of
                     Nothing => Nothing
                     (Just x) => Just (index x xs)
