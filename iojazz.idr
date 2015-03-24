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
