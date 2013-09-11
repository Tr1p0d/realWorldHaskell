import Control.Monad ( filterM )
import System.Directory ( Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import System.FilePath ((</>), takeExtension)
import Control.Exception (bracket, handle, IOException )
import System.IO (IOMode(..), hClose, hFileSize, openFile)
import RecursiveContents (getRecursiveContents)

type Predicate = FilePath
	-> Permissions
	-> Maybe Integer
	-> ClockTime
	-> Bool

type InfoP a = FilePath
	-> Permissions
	-> Maybe Integer
	-> ClockTime
	-> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k  = \w x y z -> f w x y z `q` k

liftP2 :: ( a -> b -> c ) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g = \w x y z -> f w x y z `q` g w x y z 

andP = liftP2 (&&)
orP = liftP2 (||)

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
	where check name = do
		perms <- getPermissions name
		size <- getFileSize name
		modified <- getModificationTime name
		return ( p name perms size modified )

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle ((\_ -> return Nothing) :: IOException -> IO (Maybe Integer)) $
	bracket ( openFile path ReadMode ) hClose getSize
	where
		getSize h = do
			size <- hFileSize h
			return (Just size)

myTest path _ (Just size) _ = takeExtension path == ".cpp" && size > 131072
myTest _ _ _ _ = False

liftPath :: (FilePath -> a) -> InfoP a
liftPath f = \w x y z -> f w 

myTest2 = (liftPath takeExtension ==? ".cpp") &&? (sizeP >? 131072)

(==?) = equalP
(&&?) = andP
(>?) = greaterP


