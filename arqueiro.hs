data Tree a = Null
	| Node String (Tree a) (Tree a) 

historyTree :: Tree [Char]
historyTree = 
  Node "Arqueiro"
    (Node "floresta" (Node "dentro" (Node "cipos" Null Null) (Node "escadaria" Null Null)) (Node "fora" Null Null))
    (Node "montanha" (Node "dentro" Null Null) (Node "fora" (Node "escalando" Null Null) (Node "estilingue" Null Null)))
    

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
  let montain = (breakTree historyTree "right") -- Tree: montain
  let forest = (breakTree historyTree "left") -- Tree: forest
  let withinCityBones =  (breakTree forest "left") -- Tree: within the city of bones
  let outsideCityBones =  (breakTree forest "right") -- Tree: outside the city of bones
  let withinCityDragons =  (breakTree montain "left") -- Tree: within the city of dragons
  let outsideCityDragons =  (breakTree montain "right") -- Tree: outside the city of dragons
  let climbUpVines = (breakTree withinCityBones "left") -- Tree: climb the tower by vines
  let climbUpStairs = (breakTree withinCityBones "right") -- Tree: climb the tower by stairs
  let climbUpClimbing = (breakTree outsideCityDragons "left") -- Tree: climb the tower climbing (escalando)
  let climbUpEstilingue = (breakTree outsideCityDragons "right") -- Tree: climb the tower with giant estilingue

  -- Level 1 tree
  print ("Escolha a caminhada entre " ++ (printNode forest) ++ " ou " ++ (printNode montain))
  choice <- getLine
  
  --Level 2 tree
  if choice == "floresta" then print ("Escolha seguir por " ++ (printNode withinCityBones) ++ " ou " ++ (printNode outsideCityBones) ++ " da cidade dos ossos") 
  else if choice == "montanha" then print ("Escolha seguir por " ++ (printNode withinCityDragons) ++ " ou " ++ (printNode outsideCityDragons) ++ " da cidade dos dragoes")
  else print ("Opcao Invalida")
  choice2 <-getLine
  
  --Level 3 tree
  if choice == "floresta" && choice2 == "fora" then print ("Voce perdeu!")
  else if choice == "floresta" && choice2 == "dentro" then print ("Escolha subir a torre pelos " ++ (printNode climbUpVines) ++ " ou pela " ++ (printNode climbUpStairs))
  else if choice == "montanha" && choice2 == "fora" then print ("Escolha subir a torre " ++ (printNode climbUpClimbing) ++ " ou com um " ++ (printNode climbUpEstilingue) ++ " gigante")
  else if choice == "montanha" && choice2 == "dentro" then print ("Voce perdeu!")
  else print ("Opcao invalida")
  choice3 <- getLine

  --Level 4 tree
  if choice3 == "escadaria" then print ("Parabens! Voce ganhou o jogo!!") 
  else print ("Voce perdeu o jogo!")
  
 

  return ()
  
