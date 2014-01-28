instance Functor ((->) a) where
	fmap f g = (\x -> f (g x))

instance Functor (Either a) where
	fmap f (Right a) = Right (f a)
	fmap f (Left a) = Left  (a) 

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor (CMaybe) where
	fmap f (CNothing) = CNothing
	fmap f (CJust a b) = CJust (a+1) (f b)


