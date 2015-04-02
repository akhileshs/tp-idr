module tp1

{- equality has definition as 
  data (=) : a -> b -> Type where
    Refl : x = x

  e.g. threePlusThree : 3 + 3 = 6
       threePlusThree = Refl
-}

-- first trivial proofs
plusReduces : (n: Nat) -> plus Z n = n

plusReducesS : (n: Nat) -> (m: Nat) -> S (plus n m) = plus n (S m)

-- totality checks
empty1 : Void         -- partially defined
empty1 = hd [] where
  hd : List a -> a
  hd (x :: xs) = x

empty2 : Void
empty2 = empty2       -- non terminating

total empty2 : Void
empty2 = empty2

-- simple quick sort proof
total
qsort : Ord a => List a -> List a
qsort [] = []
qsort (x :: xs) = 
  qsort (assert_smaller (x :: xs) (filter (< x) xs)) ++
    (x :: qsort (assert_smaller (x :: xs) (filter (>= x) xs)))


-- verified binary conversion
data Binary : Nat -> Type where
  bEnd : Binary Z
  b0 : Binary n -> Binary (n + n)
  bI : Binary n -> Binary (S (n + n))

data Parity : Nat -> Type where
   Even : Parity (n + n)
   Odd : Parity (S (n + n))

-- doesn't unify the definition
parity : (n: Nat) -> Parity n
parity Z = Even {n=Z}
parity (S Z) = Odd {n=Z}
parity (S (S k)) with (parity k)
  parity (S (S (j + j))) | Even = Even {n=S j}
  parity (S (S (S (j + j)))) | Odd = Odd {n=S j}

natToBin : (n: Nat) -> Binary n
natToBin Z = bEnd
natToBin (S k) with (parity k)
  natToBin (S (j + j)) | even = bI (natToBin j)
  natToBin (S (S (j + j))) | odd ?= b0 (natToBin (S j))



