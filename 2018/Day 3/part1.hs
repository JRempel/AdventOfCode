import System.Environment
import Data.List
import Data.Char
import Data.IntMap.Strict as IntMap (IntMap, empty, insertWith, foldl)

data Claim = Claim {
	id :: Int,
	left :: Int,
	top :: Int,
	width :: Int,
	height :: Int
} deriving (Show)

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
	let claims = map readClaim (split contents)
	print (calc claims)

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

readClaim :: String -> Claim
readClaim str = Claim i l t w h
	where (i : l : t : w : h : _) = splitLine str []

splitLine :: String -> [Int] -> [Int]
splitLine str result
	| str == "" = reverse result
	| otherwise = splitLine rest ((read new :: Int) : result)
	where (_, part)   = break isDigit str
	      (new, rest) = break (not . isDigit) part

fsize :: Int
fsize = 2000

ctoi :: Claim -> [Int]
ctoi claim = [fsize * (t + row) + l + col | row <- [0 .. h], col <- [0 .. w]]
	where t = top claim
	      l = left claim
	      h = height claim - 1
	      w = width claim - 1

calc :: [Claim] -> Int
calc claims = calc' claims empty

calc' :: [Claim] -> IntMap Int -> Int
calc' [] claimMap = foldM claimMap
calc' (x : rest) claimMap  = calc' rest claimMap'
	where claimMap' = upsertMulti claimMap (ctoi x)

foldM :: IntMap Int -> Int
foldM m = IntMap.foldl (\acc val  -> if val == 1 then acc else 1 + acc) 0 m

upsertMulti :: IntMap Int -> [Int] -> IntMap Int
upsertMulti m [] = m
upsertMulti m (x : xs) = upsertMulti m' xs
	where m' = upsertM m x

upsertM :: IntMap Int -> Int -> IntMap Int
upsertM m coord = insertWith (\new old -> 1 + old) coord 1 m
