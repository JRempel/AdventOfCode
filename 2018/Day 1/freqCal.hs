import Data.Typeable

main = do
	contents <- readFile "frequencies.txt"
	print (sumList (map strToInt(split contents)))

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
