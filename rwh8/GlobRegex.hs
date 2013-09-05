module GlobRegex
(
	globToRegex,
	matchesGlob
) where

import Text.Regex.Posix((=~))

type GlobError = String

globToRegex :: String -> Either GlobError String
globToRegex cs = case globToRegex' cs of
	Right result 	-> Right ('^' : result ++ "$")
	Left error		-> Left error

globToRegex' :: String -> Either GlobError String
globToRegex' "" 			= Right ""
globToRegex' ('*':cs) 		= case globToRegex' cs of
								Right result 	-> Right (".*" ++ result)
								Left error		-> Left error
globToRegex' ('?':cs) 		= case  globToRegex' cs of 
								Right result	-> Right ('.' : result)
								Left error		-> Left error
globToRegex' ('[':'!':c:cs) = case charClass cs of
								Right result 	-> Right ("[^" ++ c : result)
								Left error		-> Left error
globToRegex' ('[':c:cs) 	= case charClass cs of
								Right res	-> Right ('[' : c : res)
								Left err	-> Left err
globToRegex' ('[':_)		= Left "unterminated character class"
globToRegex' (c:cs)			= case globToRegex' cs of
								Right res	-> Right (escape c ++ res)
								Left err	-> Left err

escape :: Char -> String
escape c | elem c regex = '\\' : [c]
         | otherwise = [c]
	where regex = "\\+()^$.{}]|"

charClass :: String -> Either GlobError String
charClass (']':cs) 			= case globToRegex' cs of
								Right result 	-> Right (']' : result) 
								Left error 		-> Left error
charClass (c:cs)			= case charClass cs of
								Right res	-> Right (c : res)
								Left err	-> Left err
charClass []				= Left "unterminated character class"

matchesGlob :: FilePath -> String -> Bool
name `matchesGlob` pat = case globToRegex pat of
	Right res 	-> (name =~ res)
	Left err 	-> False
