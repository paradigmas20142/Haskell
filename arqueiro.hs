data ArvBin a = Null
	| No String (ArvBin a) (ArvBin a)

searchString :: ArvBin a -> String -> Bool
searchString Null value = False
searchString (No x left right) valor 
	| value == x = True
 	| otherwise = False || (searchString left value) || (searchString right value)
