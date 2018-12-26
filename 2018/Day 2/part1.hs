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
	let result = checkSum (split contents)
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

checkSum :: [String] -> Int
checkSum strs = twos * threes
	where nums   = map check strs
	      twos   = countOf nums 2
	      threes = countOf nums 3

countOf :: [[Int]] -> Int -> Int
countOf nums val = length (filter (elem val) nums)

check :: String -> [Int]
check str = nub (filterNum (map reduce (zipSelf str)))

filterNum :: [Int] -> [Int]
filterNum xs = filter (\n -> n == 2 || n == 3) xs

reduce :: (Char, String) -> Int
reduce (char, str) = length (elemIndices char str)

zipSelf :: String -> [(Char, String)]
zipSelf src = zip src (take (length src) (repeat src))
