import Data.Vect

insert : Ord a => (x : a) -> (xsSorted : Vect len a) -> Vect (S len) a
insert x [] = [x] --?insert_rhs_2
insert x (y :: xs) = case x < y of
                          False => y :: insert x xs
                          True => x :: y :: xs

insSort : Ord a => Vect len a -> Vect len a
insSort [] = []
insSort (x :: xs) =
  let
    xsSorted = insSort xs
  in
    insert x xsSorted
