module Glob (namesMatching) where

import System.Directory ( 	doesDirectoryExists, doesFileExists, 
							getCurrentDirectory, getDirectoryContents	)
import System.Filepath (	dropTrailingPathSeparator, splitFileName, (</>))
import Control.Exception ( 	handle )
import Control.Monad 	( forM )
import GlobRegex (	matchesGlob	)

isPattern :: String -> Bool
isPattern = any (`elem` "[?*")





