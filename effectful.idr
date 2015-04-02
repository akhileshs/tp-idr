module effectful

import Effects
import Effect.State

data BTree a = Leaf
             | Node (BTree a) a (BTree a)

instance Show a => Show (BTree a) where
  show Leaf = "[]"
  show (Node l x r) = "[" ++ show l ++ " "
                          ++ show x ++ " "
                          ++ show r ++ "]"
                        
trialTree : BTree String
trialTree = Node (Node Leaf "aki" Leaf)
              "adi"
              (Node (Node Leaf "ari" Leaf))

treeTagAux : BTree a -> { [State Int] } Eff (BTree (Int, a))
treeTagAux Leaf = pure Leaf
treeTagAux (Node l x r) = do l' <- treeTagAux l
                             i <- get
                             put (i + 1)
                             r' <- treeTagAux r
                             pure (Node l' (i, x) r')

treeTag : (i: Int) -> BTree a -> BTree (Int, a)
treeTag i x = runPure (do put i, treeTagAux x)

main : IO ()
main = print (treeTag 1 trialTree)
