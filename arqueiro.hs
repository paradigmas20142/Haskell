data Tree a = Null
	| Node String (Tree a) (Tree a) 

historyTree :: Tree [Char]
historyTree = 
  Node "Arqueiro"
    (Node "Montanha" (Node "dentro" Null Null) (Node "fora" Null Null))
    (Node "Floresta" (Node "dentro" Null Null) (Node "fora" Null Null))

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
	| value == "all" = (Node a left right)

printNode :: Tree [Char] -> [Char]
printNode Null = ""
printNode (Node a left right) = a

main :: IO ()	
main = do
  let holdLeft = (breakTree historyTree "left")
  let holdRight = (breakTree historyTree "right")
  print ("Escolha a caminhada entre " ++ (printNode holdLeft) ++ " ou " ++ (printNode holdRight))
  choice <- getLine
  let fisrtTreeSecondLevel =  (breakTree holdLeft "left")
  let secondTreeSecondLevel =  (breakTree holdLeft "right")
  let thirdTreeSecondLevel =  (breakTree holdRight "left")
  let fourthTreeSecondLevel =  (breakTree holdRight "right")
  if choice == "Montanha"
  then print ("Escolha seguir por " ++ (printNode fisrtTreeSecondLevel) ++ " ou " ++ (printNode secondTreeSecondLevel) ++ " da cidade dos dragoes")
  else print ("Escolha seguir por " ++ (printNode thirdTreeSecondLevel) ++ " ou " ++ (printNode fourthTreeSecondLevel) ++ " da cidade dos ossos")
  
  return ()
  
