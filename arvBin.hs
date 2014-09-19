import System.IO
data ArvBin a = Null | No a (ArvBin a) (ArvBin a) 

historyTree :: ArvBin [Char]
historyTree = 
  No "Guerreiro"
    (No "Floresta" Null Null)
    (No "Montanha" Null Null)
 


-- singleton a = Node x Null Null
--
-- goTo a (No a esq dir)
-- goTo a CidadeOssos = singleton a
-- goTo 
--
--
-- insertTree x (No a esq dir)
--   |x == "Floresta"  = No x esq dir
--   |x == "Montanha" = No x esq dir
--
-- main = do
--   
--   handle <- openFile "warrior.txt" ReadMode
--   contents <- hGetContents handle
--   hClose handle

showNode :: ArvBin [Char] -> [Char]
showNode Null = "Cidade dos Ossos"
showNode (No x esq dir) = x ++ (showNode esq) ++ (showNode dir)

