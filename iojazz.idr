-- type IO represented as 
-- data IO a

greet : IO ()
greet = do putStr "What's up! Name please?"
           name <- getLine
           putStrLn ("Hello" ++ name)

-- map analogue in Idris
dmap : (a -> b) -> List a -> List b
dmap f []        = []
dmap f (x :: xs) = f x :: dmap f xs

dmap : (a -> b) -> Vect n a -> Vect n b
dmap f [] = []
dmap f (x :: xs) = f x :: map f xs

-- for example
intVec : Vect 5 Int
intVec = [1, 2, 3, 4, 5]

double : Int -> Int
double x = x * 2

-- can write something like 
-- show (map double intVec)

-- Idris' equivalent of Scala's Option type
data Maybe a = Just a | Nothing

-- consider list lookup
lst_lookup : Nat -> List a -> Maybe a
lst_lookup _  Nil       = Nothing
lst_lookup Z (x :: xs)  = Just x
lst_lookup (S k) (x :: xs) = lst_lookup k xs

-- dependent pairs
data Sigma : (A : Type) -> (P : A -> Type) -> Type where
  MkSigma : {P : A -> Type} -> (a : A) -> P a -> Sigma A P
 
-- type of the second element depends of first element's value

-- onwards to list comprehensions
pyth : Int -> List (Int, Int, Int)
pyth n = [ (x, y, z) | z <- [1..n], y <- [1..z], x <- [1..y],
                       x*x + y*y == z*z ]

-- dependent records
record Person : Type where
  MkPerson : (name : String) ->
             (age : Int) -> Person

akhilesh : Person
akhilesh = MkPerson "Akhilesh" 21

