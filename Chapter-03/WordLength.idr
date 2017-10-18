allLengths : List String -> List Nat
allLengths [] = []
allLengths (word :: words) = length word :: allLengths words


xor : Bool -> Bool -> Bool
xor False y = y
xor True y = not y


isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

mutual
  is_even : Nat -> Bool
  is_even Z = True
  is_even (S k) = is_odd k

  is_odd : Nat -> Bool
  is_odd Z = False
  is_odd (S k) = is_even k
