module typeClassFun

class tShow a where
  tshow : a -> String

-- read as 
-- "under constraint that tshow is an instance of tShow, take a and give me a string

instance tShow a => tShow (Vect n a) where
  tshow xs = "[" ++ tshow' xs ++ "]" where
    tshow' : Vect n a -> String
    tshow' Nil        = ""
    tshow' (x :: Nil) = tshow x
    tshow' (x :: xs)  = tshow x ++ ", " ++ tshow' xs

-- more examples
data Ordering = LT | EQ | GT

class Eq a => Ord a where
  compare : a -> a -> Ordering
  (<) : a -> a -> Bool
  (>) : a -> a -> Bool
  (<=) : a -> a -> Bool
  (>=) : a -> a -> Bool
  max : a -> a -> a
  min : a -> a -> a

sort : Ord a => List a -> List a
sortAndShow : (Ord a, Show a) => List a -> String
sortAndShow xs = show (sort xs)

