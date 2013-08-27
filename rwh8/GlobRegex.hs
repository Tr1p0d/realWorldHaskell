module GlobRegex
(
	globToRegex,
	matchesGlob
) where

import Text.Regex.Posix((=~))

globToRegex :: String -> String
globToRegex cs = '^' : globToRegex' cs ++ "$"

globToRegex' :: String -> String
globToRegex' "" = ""
globToRegex' ('*':cs) 		= ".*" ++ globToRegex' cs
globToRegex' ('?':cs) 		= '.' : globToRegex' cs
globToRegex' ('[':'!':c:cs) = "[^" ++ c : charClass cs
globToRegex' ('[':c:cs) 	= '[' : c : charClass cs
globToRegex' ('[':_)		= error "unterminated character class"
globToRegex' (c:cs)			= escape c ++ globToRegex' cs

escape :: Char -> String
escape c | elem c regex = '\\' : [c]
         | otherwise = [c]
	where regex = "\\+()^$.{}]|"

charClass :: String -> String
charClass (']':cs) 			= ']' : globToRegex' cs
charClass (c:cs)			= c : charClass cs
charClass []				= error "unterminated character class"

matchesGlob = "12#"
