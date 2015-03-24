module dtStart

-- simple examples of dependent types
-- Here DVect is a type dependent on the value of the length
data DVect : Nat -> Type -> Type where
  DNil : DVect Z a
  (::) : a -> DVect k a -> DVect (S k) a

(++) : DVect n a -> DVect m a -> DVect (n + m) a
(++)       DNil ys = ys
(++) (x :: xs) ys = x :: xs ++ ys

data DFin : Nat -> Type where
  DFZ : DFin (S k)
  DFS : DFin k -> DFin (S k)

dindex : DFin n -> DVect n a -> a
dindex DFZ     (x :: xs) = x
dindex (DFS k) (x :: xs) = dindex k xs

{-
  dindex could've been written as 
  dindex : {a: Type} -> {n: Nat} -> DFin n -> DVect n a -> a
  a, n are implicit in this case
-}






