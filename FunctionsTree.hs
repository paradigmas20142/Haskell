module FunctionsTree(historyTree, breakTree, printNode) where

data Tree a = Null
	| Node String (Tree a) (Tree a) 

historyTree :: Tree [Char]
historyTree = 
  Node "Arqueiro"
    (Node "floresta" 
	(Node "dentro" 
		(Node "cipos" Null Null) 
		(Node "escadaria" Null Null)) 
	(Node "fora" Null Null))
    (Node "montanha" 
	(Node "dentro" Null Null) 
	(Node "fora" 
		(Node "escalando" Null Null) 
		(Node "estilingue" Null Null)))
  
breakTree :: Tree [Char] -> String -> Tree [Char]
breakTree Null value = Null
breakTree (Node a left right) value
	| value == "right" = right
	| value == "left" = left
	| value == "all" = (Node a left right)

printNode :: Tree [Char] -> [Char]
printNode Null = ""
printNode (Node a left right) = a
