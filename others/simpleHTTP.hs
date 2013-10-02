import Network.HTTP

testRide url = simpleHTTP req 
where
	req = mkRequest GET parsedUri
	parsedUri = parseURI url 
