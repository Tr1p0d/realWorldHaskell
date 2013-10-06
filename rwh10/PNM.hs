import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char


data Greymap = Greymap {
	greyWidth :: Int
,	greyHeight :: Int
,	maxVal :: Int
,	greyData :: L.ByteString
} deriving ( Eq )

instance Show Greymap where
	show (Greymap w h _ _) = "GreyMap : " ++ show w ++ " x " ++ show h

--parser start

matchHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
matchHeader pattern inData
	| pattern`L8.isPrefixOf`inData = Just (L8.dropWhile isSpace (L.drop (L.length pattern) inData)) 
	| otherwise = Nothing

getNat :: L.ByteString -> Maybe (Int, L.ByteString)
getNat str = case L8.readInt str of
	Nothing -> Nothing
	Just (num, rest)
		| num <= 0 -> Nothing
		| otherwise -> Just (num, rest)


getBytes :: Int -> L.ByteString -> Maybe(L.ByteString, L.ByteString)
getBytes num str = if L.length prefix < num then Nothing else Just both
	where
		both@(prefix,_) = L.splitAt num str

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 str = 
	case matchHeader (L8.pack "P5") str of --check header
		Nothing -> Nothing
		Just s1 -> 
			case getNat s1 of -- get width
				Nothing -> Nothing
				Just (width, s2) -> 
					case getNat (L8.dropWhile isSpace s2) of --get height
						Nothing -> Nothing
						Just (height, s3) -> 
							case getNat (L8.dropWhile isSpace s2) of -- get max grey
								Nothing -> Nothing
								Just (maxVal, s4)
									| maxVal > 255 -> Nothing
									| otherwise -> --get rid of whitespace
									case getBytes 1 s4 of
										Nothing -> Nothing	
										Just (_, rest) -> 
											case getBytes (width*height) rest of
												Nothing -> Nothing
												Just (done, left) -> --final string
													Just (Greymap width height maxVal done, left)
										

						

