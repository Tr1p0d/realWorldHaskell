module PrettyJson
(
simpleEscapes
) where

import SimpleJSON

data Doc = ToBeDefined deriving ( Show )


renderJValue (JBool True) 	= text "true"
renderJValue (JBool False) 	= text "false"
renderJValue (JNull)		= text "null"
renderJValue (JNumber num)	= double num
renderJValue (JString str)	= string str

--string :: String -> Doc
--string str = enclose '"' '"' . hcat . map oneChar
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

--enclose :: Char -> Char -> Doc -> Doc
--enclose right left x = char right <> x <> char left 

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Doc -> Doc
char c = undefined 

--oneChar :: Char -> Doc
--oneChar c = case lookup c simpleEscapes of
--	Just r -> text r
--	Nothing | mustEscape c 	-> hexEscape c
--			| otherwise 	-> char c
--	where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = ( a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) 'o') <> text h
	where h = showHex x ""
