module Main where

import MainArcher(mainArcher)
import MainWarrior(mainArrior)

main :: IO ()
main = do
	print ("Escolha um jogador")
	player <- getLine
	if player == "arqueiro" then mainArcher
	else if player == "guerreiro" then print ("Main guerreiro")
	else print ("Opcao invalida")
