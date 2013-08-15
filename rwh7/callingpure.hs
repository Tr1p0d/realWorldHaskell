
name2reply str = "nice to meet you " ++ str ++ "\n"
	++ "your name is " ++ show (length str) ++ "long"

main :: IO()
main = do
	putStrLn "nazdar"
	inpStr <- getLine
	putStrLn (name2reply inpStr)
