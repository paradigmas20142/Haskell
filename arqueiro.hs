data Tree a = Null
	| Node String (Tree a) (Tree a) 

historyTree :: Tree [Char]
historyTree = 
  Node "Arqueiro"
    (Node "Montanha" Null Null)
    (Node "Floresta" Null Null)

searchString :: Tree [Char] -> [Char] -> [Char]
searchString Null value = ""
searchString (Node x left right) value 
	| value == x = x
 	| value /= x = (searchString left value)

breakTree :: Tree [Char] -> String -> Tree [Char]
breakTree Null value = Null
breakTree (Node a left right) value
	| value == "right" = right
	| value == "left" = left

printNode :: Tree [Char] -> [Char]
printNode Null = ""
printNode (Node a left right) = a

	
main = do
  let holdLeft = (breakTree historyTree "left")
  let holdRight = (breakTree historyTree "right")
  print ("Escolha a caminhada entre " ++ (printNode holdLeft) ++ " ou " ++ (printNode holdRight))

  
  
