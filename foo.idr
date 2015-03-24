module fooing

foo : Int -> Int
foo x = case isLT of
             Yes => x * 2
             No  => x * 4
        where
          data MyLT = Yes | No

          isLT : MyLT
          isLT = if x < 10 then Yes else No

even : Nat -> Bool
even Z = True
even (S k) = odd k where 
  odd Z = False
  odd (S k) = even k

test : List Nat
test = [c (S 1), c Z, d (S Z)]
  where
    c x = 42 + x
    d y = c (y + 1 + z y)
    where
      z w = y + w

