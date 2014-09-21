data Tree a = Null
	| Node String (Tree a) (Tree a) 

historyTree :: Tree [Char]
historyTree = 
  Node "Arqueiro"
    (Node "montanha" (Node "dentro" Null Null) (Node "fora" (Node "escalando" Null Null) (Node "estilingue" Null Null)))
    (Node "floresta" (Node "dentro" (Node "cipos" Null Null) (Node "escadaria" Null Null)) (Node "fora" Null Null))

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
  let montain = (breakTree historyTree "left") -- Tree: montain
  let forest = (breakTree historyTree "right") -- Tree: forest
  let withinCityBones =  (breakTree montain "left") -- Tree: within the city of bones
  let outsideCityBones =  (breakTree montain "right") -- Tree: outside the city of bones
  let withinCityDragons =  (breakTree forest "left") -- Tree: within the city of dragons
  let outsideCityDragons =  (breakTree forest "right") -- Tree: outside the city of dragons
  let climbUpVines = (breakTree withinCityBones "left") -- Tree: climb the tower by vines
  let climbUpStairs = (breakTree withinCityBones "right") -- Tree: climb the tower by stairs
  let climbUpClimbing = (breakTree outsideCityDragons "left") -- Tree: climb the tower climbing (escalando)
  let climbUpEstilingue = (breakTree outsideCityDragons "right") -- Tree: climb the tower with giant estilingue


  print ("Escolha a caminhada entre " ++ (printNode montain) ++ " ou " ++ (printNode forest))
  choice <- getLine
  
  if choice == "montanha" then print ("Escolha seguir por " ++ (printNode withinCityBones) ++ " ou " ++ (printNode outsideCityBones) ++ " da cidade dos dragoes") 
  else if choice == "floresta" then print ("Escolha seguir por " ++ (printNode withinCityDragons) ++ " ou " ++ (printNode outsideCityDragons) ++ " da cidade dos ossos")
  else print ("Opcao Invalida")
  choice2 <-getLine

  if choice == "montanha" && choice2 == "dentro" then print ("Voce perdeu!")
  else if choice == "montanha" && choice2 == "fora" then print ("Escolha subir a torre " ++ (printNode climbUpClimbing) ++ " ou com um " ++ (printNode climbUpEstilingue) ++ " gigante")
  else if choice == "floresta" && choice2 == "dentro" then print ("Escolha subir a torre pelos " ++ (printNode climbUpVines) ++ " ou pela " ++ (printNode climbUpStairs))
  else if choice == "floresta" && choice2 == "fora" then print ("Voce perdeu!")
  else print ("Opcao invalida")
  choice3 <- getLine

  
  
 

  return ()
  
