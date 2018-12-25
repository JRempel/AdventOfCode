import System.Environment

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
	let result = (sumList . map strToInt . split) contents 
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

strToInt :: String -> Integer
strToInt ('-' : num) = (-1) * read num
strToInt ('+' : num) = read num

sumList :: [Integer] -> Integer
sumList list = sumList' 0 list

sumList' :: Integer -> [Integer] -> Integer
sumList' count [] = count
sumList' count (x : rest) = sumList' newCount rest
	where newCount = count + x
