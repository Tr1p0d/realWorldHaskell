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

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = 	Just 3 >>= (\x ->
	Just (show x ++ "koruny") >>= (\y ->
	Just (show y ++ "!")))

foo2 :: Maybe String
foo2 = 	Just 9 >>= (\x ->
	Nothing >>= (\y ->
	Just (show x ++ y)))

tt = do
	x <- Nothing
	Just (x > 8)

routine :: Maybe Pole
routine = return (0,0) >>= landRight 2 >>= landRight 2 >>= landRight 2

routine2 = do 
	start <- return (0,0)
	right <- landRight 2 start
	left <- landLeft 3 right
	Just left


