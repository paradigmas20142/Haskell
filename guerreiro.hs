data Tree a = Empty 
  | Node a (Tree a) (Tree a) deriving (Show) 

historyTree :: Tree [Char]
historyTree =   
  Node "Guerreiro"  
    (Node "Montanha"  Empty Empty)  
    (Node "Floresta"   Empty Empty)  

runHistory :: Tree [Char] ->  [Char]
runHistory Empty = ""
runHistory (Node a left right) = a ++ (runHistory left) ++ (runHistory right)

main = do 
  print (runHistory historyTree)
