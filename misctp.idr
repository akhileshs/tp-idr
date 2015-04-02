module misctp

import Effects
import Effect.StdIO
-- miscellaneous proofs
isCons : List a -> Bool
isCons [] = False
isCons (x :: xs) = True

-- auto allows automatic proof construction. implicit argument is filled by searching through the context.
head : (xs : List a) -> (auto p : isCons xs = True) -> a
head (x :: xs) _ = x

factorial : Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))

-- basic type provider jazz
strToType : String -> Type
strToType "Int" = Int
strToType _ = Nat

fromFile : String -> IO (Provider Type)
fromFile fname = do str <- readFile fname
                    return (Provide (strToType (trim str)))

hello : [STDIO] Eff ()
hello = putStrLn "Hello, World!"

main : IO ()
main = run hello
