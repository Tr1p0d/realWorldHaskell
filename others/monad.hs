applyMaybe :: Maybe a -> ( a -> Maybe b ) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just a) f = f a

type Birds = Int
type Pole = (Birds, Birds)

x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft b (l ,r) 
	| abs(l - r) >= 4 = Nothing
	| otherwise = Just (l + b, r)
 
landRight :: Birds -> Pole -> Maybe Pole
landRight b (l ,r) 
	| abs(l - r) >= 4 = Nothing
	| otherwise = Just (l, r + b)
