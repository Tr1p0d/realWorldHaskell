import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catch)
import Control.Exception (finally)

main :: IO
main = withTempFile "myTemp.txt" myAction

tellPosition :: Handle -> FilePath -> IO
tellPosition h p = do
	pos <- hTell h
	putStrLn "our position on file : " ++ p ++ " is : " ++ pos
	

myAction :: FilePath -> Handle -> IO
myAction tempname temph = do
	putStrLn "welcome to write tmpFile.hs"
	tellPosition temph tempname

	putStrLn "lets write something"

	putStrLn "written"
	
withTempFile :: FilePath -> ( FilePath -> Handle -> IO a ) -> IO a
withTempFile pattern func = do
	




