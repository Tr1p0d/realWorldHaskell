import Control.Monad ( filterM, forM, mapM, liftM )
import System.Directory ( Permissions(..), getModificationTime, getPermissions)
import System.FilePath ((</>), takeExtension)
import Control.Exception (bracket, handle, IOException )
import System.Directory ( doesDirectoryExist, getDirectoryContents )
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import System.Time (ClockTime(..))

data Info = Info {
	infoPath :: FilePath,
	infoPerms :: Maybe Permissions,
	infoSize :: Maybe Integer,
	infoTime :: Maybe ClockTime
	} deriving (Eq, Ord, Show)

maybeIO :: IO a -> IO (Maybe a)
maybeIO act = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe a)) (Just `liftM` act)

getInfo :: FilePath -> IO Info
getInfo path = do
	perms <- maybeIO (getPermissions path)
	size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
	time <- maybeIO (getModificationTime path)
	return (Info path perms size time)

traverse :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverse order path = do
	names <- getUsefulContents path
	contents <- mapM getInfo (path : map (path </>) names)
	liftM concat $ forM (order contents) $ \info -> do
		if isDirectory info && infoPath info /= path
		then traverse order (infoPath info)
		else return [info]

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
	names <- getDirectoryContents path
	return (filter (`notElem` [".", ".."]) names)

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms
