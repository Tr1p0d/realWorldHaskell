import System.IO
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO.Error (catch)
import Control.Exception (finally)

main :: IO () 
main = withTempFile "myTemp.txt" myAction

tellPosition :: Handle -> FilePath -> IO String
tellPosition h p = do
	pos <- hTell h
	return $ "our position on file : " ++ p ++ " is : " ++ (show pos)
	

myAction :: FilePath -> Handle -> IO ()
myAction tempname temph = do
	putStrLn "welcome to write tmpFile.hs"
	tmp <- tellPosition temph tempname
	putStrLn tmp

	putStrLn "lets write something"
	hPutStrLn temph $ show [1..100]
	putStrLn "written"

	tmp <- tellPosition temph tempname
	putStrLn tmp
	putStrLn "done"

	
withTempFile :: FilePath -> ( FilePath -> Handle -> IO a ) -> IO a
withTempFile pattern func = do
	tempdir <- catch (getTemporaryDirectory) (\_ -> return ".")	
	(tempfile, temph) <- openTempFile tempdir pattern
	finally (func tempfile temph) (do 
		hClose temph
		removeFile tempfile)




