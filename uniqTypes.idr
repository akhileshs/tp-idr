module uniqTypes

-- unique lists shown below
data UList : Type -> UniqueType
     Nil : UList a
     (::) : a -> UList a -> UList a

umap : (a -> b) -> UList a -> UList b
umap f [] = []
umap f (x :: xs) = f x :: umap f xs

-- invalid bad list type
data BadList : UniqueType -> Type
     Nil : {a : UniqueType} -> BadList a
     (::) : {a : UniqueType} -> a -> BadList a -> BadList a

-- basic borrowed types
data Borrowed : UniqueType -> BorrowedType where
  Read : {a : UniqueType} -> a -> Borrowed a

implicit 
lend : {a : UniqueType} -> a -> Borrowed a
lend x = Read x

showU : Show a => Borrowed (UList a) -> String
showU xs = "[" ++ showU' xs ++ "]" where
  showU' : Borrowed (UList a) -> String
  showU' [] = ""
  showU' [x] = show x
  showU' (x :: xs) = show x ++ ", " ++ showU' xs
