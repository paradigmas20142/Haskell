module MainWarrior(mainWarrior) where

import System.IO
import Control.Monad
data Tree a = Empty
  | Node String (Tree a) (Tree a) 


historyTree :: Tree [Char]
historyTree = 
  Node "Guerreiro"
  (Node "montanha" (Node "dentro" (Node "escalando" Empty Empty) (Node "estilingue" Empty Empty)) (Node "fora" Empty Empty ))
  (Node "floresta" (Node "dentro" Empty Empty) (Node "fora" (Node "cipos" Empty Empty) (Node "escadaria" Empty Empty)))

searchString :: Tree [Char] -> [Char] -> [Char]
searchString Empty value = ""
searchString (Node x left right) value 
  | value == x = x
  | value /= x = (searchString left value)

breakTree :: Tree [Char] -> String -> Tree [Char]
breakTree Empty value = Empty
breakTree (Node a left right) value
  | value == "right" = right
  | value == "left" = left
  | value == "all" = (Node a left right)

printNode :: Tree [Char] -> [Char]
printNode Empty = ""
printNode (Node a left right) = a

mainWarrior :: IO () 
mainWarrior = do
  let montain = (breakTree historyTree "left") -- Tree: montain
  let forest = (breakTree historyTree "right") -- Tree: forest
  let withinCityBones =  (breakTree forest "left") -- Tree: within the city of bones
  let outsideCityBones =  (breakTree forest "right") -- Tree: outside the city of bones
  let withinCityDragons =  (breakTree montain "left") -- Tree: within the city of dragons
  let outsideCityDragons =  (breakTree montain "right") -- Tree: outside the city of dragons
  let climbUpVines = (breakTree outsideCityBones "left") -- Tree: climb the tower by vines
  let climbUpStairs = (breakTree outsideCityBones "right") -- Tree: climb the tower by stairs
  let climbUpClimbing = (breakTree withinCityDragons "left") -- Tree: climb the tower climbing (escalando)
  let climbUpEstilingue = (breakTree withinCityDragons "right") -- Tree: climb the tower with giant estilingue


  print ("Escolha a caminhada entre " ++ (printNode montain) ++ " ou " ++ (printNode forest))
  choice <- getLine

  if choice == "montanha" then print ("Escolha seguir por " ++ (printNode withinCityDragons) ++ " ou " ++ (printNode outsideCityDragons) ++ " da cidade dos dragoes") 
  else if choice == "floresta" then print ("Escolha seguir por " ++ (printNode withinCityBones) ++ " ou " ++ (printNode outsideCityBones) ++ " da cidade dos ossos")
  else print ("Opcao Invalida")
  choice2 <-getLine
  if choice == "montanha" && choice2 == "fora" then print ("Voce perdeu!")
  else if choice == "montanha" && choice2 == "dentro" then print ("Escolha subir a torre " ++ (printNode climbUpClimbing) ++ " ou com um " ++ (printNode climbUpEstilingue) ++ " gigante")
  else if choice == "floresta" && choice2 == "fora" then print ("Escolha subir a torre pelos " ++ (printNode climbUpVines) ++ " ou pela " ++ (printNode climbUpStairs))
  else if choice == "floresta" && choice2 == "dentro" then print ("Voce perdeu!")
  else print ("Opcao invalida")
  choice3 <- getLine

  if choice3 == "cipos" then print ("Parabens! Voce ganhou o jogo!!") 
  else print ("Voce perdeu o jogo!!") 

  return ()
