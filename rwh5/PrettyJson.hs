module PrettyJson
(
simpleEscapes ,
smallHex ,
astral
) where

import SimpleJSON
import Numeric
import Data.Bits
import Data.Char (ord)

data Doc = ToBeDefined deriving ( Show )


renderJValue (JBool True) 	= text "true"
renderJValue (JBool False) 	= text "false"
renderJValue (JNull)		= text "null"
renderJValue (JNumber num)	= double num
renderJValue (JString str)	= string str
renderJValue (JArray ary) 	= series '[' ']' renderJValue ary
renderJValue (JObject obj)	= series '{' '}' field obj
	where 
		field ( name, val ) = string name <> text ": " <> renderJValue val

--string :: String -> Doc
--string str = enclose '"' '"' . hcat . map oneChar
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

enclose :: Char -> Char -> Doc -> Doc
enclose right left x = char right <> x <> char left 

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = Char c 

oneChar :: Char -> Doc
oneChar c = case lookup c simpleEscapes of
	Just r -> text r
	Nothing | mustEscape c 	-> hexScape c
			| otherwise 	-> char c
	where mustEscape c = c < ' ' || c == '\x7f' || c > '\xff'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
	where ch a b = ( a, ['\\',b])

smallHex :: Int -> Doc
smallHex x = text "\\u" <> text (replicate (4 - length h) 'o') <> text h
	where h = showHex x ""

astral :: Int -> Doc
astral num = smallHex( a + 0xd800 ) <> smallHex ( b + 0xdc00 )
	where 
		a = ( num `shiftR` 10) .&. 0x3ff
		b = num .&. 0x3ff

hexScape :: Char -> Doc
hexScape c 
	| d < 0x10000 = smallHex d
	| otherwise   = astral ( d - 0x010000 )
		where d = ord c

series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close item = enclose open close . fsep . punctuate ( char ',' ) 
						. map item

fsep :: [Doc] -> Doc
fsep xs = undefined

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p [] = []
punctuate p [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds


