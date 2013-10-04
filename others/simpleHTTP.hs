import Network.HTTP
import Network.URI
import Data.Maybe
import Network.Stream

data HTTPErrorStructure = 
	ConnectionError |
	ParseError		|
	UnknownError	|
	Redirect		|
	NotFound		

testRide :: String -> IO String
testRide url = do
	a <- simpleHTTP req
	case a of
		Left ErrorReset -> return "asd"--(ConnectionError)
		Left ErrorClosed ->  return "asd"--(ConnectionError)
		Left (ErrorParse a) -> return "asd"--( ParseError)
		Left (ErrorMisc a) ->  return "asd"--( UnknownError)
		otherwise -> do
			b <- getResponseCode a
			case b of
				(3,_,_) -> 
					case lookupHeader HdrLocation (fromJust responseHeaders) of
						Just c -> testRide c
						Nothing -> return "got redirect but cannot redirect"
					where 	
						responseHeaders = 
							case a of
								Left x -> Nothing
								Right n -> Just (rspHeaders n)
				(2,_,_) -> do (getResponseBody a)
				(4,_,_) -> return "notfound"-- ( NotFound)
	where
		req = mkRequest GET parsedUri
		parsedUri = fromJust $ parseURI url 



-- wisLogin user pass url =
