{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverlappingInstances #-}

import Data.Char


class PrettyPrintable a where
	toPrettyPrint :: a -> String
	fromPrettyPrint :: String -> a

--instance PrettyPrintable Int where
	--toPrettyPrint :: String -> String
--	toPrettyPrint a = map toUpper $ show a

	--fromPrettyPrint :: String -> String
--	fromPrettyPrint a = read a :: Int

instance PrettyPrintable String where
	toPrettyPrint a = map toUpper a
	fromPrettyPrint a = map toLower a

--instance (PrettyPrintable a) => PrettyPrintable [a] where
--	toPrettyPrint a = concat $ map toPrettyPrint a
--	fromPrettyPrint = undefined

instance PrettyPrintable [String] where
	toPrettyPrint a = concat $ map toPrettyPrint a
	fromPrettyPrint = undefined


instance (PrettyPrintable a) => PrettyPrintable [(String, a)] where
	toPrettyPrint = undefined
	fromPrettyPrint = undefined
	
