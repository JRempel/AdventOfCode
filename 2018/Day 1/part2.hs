import System.Environment
import Data.Set as Set (Set, insert, member, singleton)	

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
	let result = (fstDupSum . map strToInt . split) contents 
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

fstDupSum :: [Integer] -> Integer
fstDupSum list = fstDupSum' 0 (cycle list) (singleton 0)

fstDupSum' :: Integer -> [Integer] -> Set Integer -> Integer
fstDupSum' count (x : rest) sums 
	| member count' sums = count'
	| otherwise          = fstDupSum' count' rest sums'
	where count' = count + x
	      sums'  = insert count' sums

