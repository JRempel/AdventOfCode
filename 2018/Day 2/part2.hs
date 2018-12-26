import System.Environment
import Data.List

main :: IO()
main = do
	args <- getArgs
	case args of
		[] -> error "Supply a filename to open."
		[arg] -> do handle arg
		_ -> error "Too many arguments." 

handle :: String -> IO()
handle fn = do
	contents <- readFile fn
	let result = findCommon (split contents)
	print result

split :: String -> [String]
split [] = []
split ('\r' : rest) = split rest
split ('\n' : rest) = split rest
split text = 
	pre : split suf
	where (pre, suf) = break isLS text

isLS :: Char -> Bool
isLS c 
	| c == '\r' = True
	| c == '\n' = True
	| otherwise = False

findCommon :: [String] -> String
findCommon list = findCommon' (sort list)

findCommon' :: [String] -> String
findCommon' (a : b : rest) = case result of
	(Just result) -> result
	Nothing       -> findCommon' (b : rest)
	where result = diffByOne a b

diffByOne :: String -> String -> Maybe String
diffByOne a b
	| len == 1  = Just removed
	| otherwise = Nothing
	where removed = a \\ diff
	      len     = length diff
	      diff    = a \\ b
