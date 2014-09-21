module MainArcher where

import FunctionsTree(historyTree, breakTree, printNode)

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
  else print ("Voce perdeu o jogo!!") 
  
  return ()
  
