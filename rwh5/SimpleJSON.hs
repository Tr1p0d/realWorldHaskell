
module SimpleJSON
	(
		JValue(..)
		, jGetBool
		, jGetInt
		, jGetDouble
		, jGetObject
		, jGetArray
		, jIsNull
		, jPutValue
	) where

import Data.List



data JValue = JString String 
	| JNumber Double
	| JNull 
	| JBool Bool
	| JObject [(String, JValue)]
	| JArray [JValue] deriving ( Eq, Ord, Show )



jGetBool (JBool j) 	= Just j
jGetBool _			= Nothing

jGetInt	(JNumber j)	= Just (truncate j)
jGetInt _			= Nothing

jGetDouble ( JNumber j ) = Just j
jGetDoublt _		= Nothing

jGetObject ( JObject k ) = Just k
jGetObject _		= Nothing

jGetArray ( JArray j )	= Just j
jGetArray _			= Nothing

jIsNull v			= v == JNull

jRenderValue :: JValue -> String
jRenderValue ( JString j ) = show j 
jRenderValue ( JNumber j ) = show j 
jRenderValue ( JBool True ) = "True"
jRenderValue ( JBool False ) = "False"
jRenderValue ( JNull )	= "JNull"
jRenderValue ( JObject j ) = "{" ++ pairs j ++ "}"
	where
	 	pairs [] = ""
		pairs ps = intercalate ", " (map renderPair ps)
		renderPair (n,v) = show n ++ " : " ++ jRenderValue v
jRenderValue ( JArray k ) = "[" ++ values k ++ "]"
	where
	 	values [] = ""
		values vs = intercalate ", " (map jRenderValue vs)

jPutValue :: JValue -> IO()
jPutValue v = putStrLn (jRenderValue v)

