import qualified Data.ByteString.Lazy as L

hasElfMagick :: L.ByteString -> Bool
hasElfMagick content = elfMagick == L.take 4 content
	where elfMagick = L.pack [0x7f, 0x45, 0x4c, 0x46]

isElfFile :: FilePath -> IO Bool
isElfFile path = do
	content <- L.readFile path
	return $ hasElfMagick content
