module Main where

import MainArcher(mainArcher)
import MainWarrior(mainWarrior)

main :: IO ()
main = do
	print ("Escolha um jogador")
	player <- getLine
	if player == "arqueiro" then mainArcher
	else if player == "guerreiro" then mainWarrior
	else print ("Opcao invalida")
