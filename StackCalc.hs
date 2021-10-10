import Data.Char
import Data.Map (fromList, (!?), Map)
import Data.Maybe (isNothing)

compose [] = id
compose (f:fs) = f . (compose fs)

binop :: (a -> a -> a) -> [a] -> [a]
binop op (x: (y: zs)) = (op y x) : zs

unop :: (a -> a) -> [a] -> [a]
unop op (x: ys) = (op x) : ys

binops :: Map String (Int -> Int -> Int)
binops = fromList [
        ("+", (+)),
        ("-", (-)),
        ("*", (*)),
        ("/", (div)),
        ("%", (mod))
    ]

unops :: Map String (Int -> Int)
unops = fromList $ map f [0..9]  
    where f d = ([intToDigit d], (\x -> x*10 + d))

specops :: Map String ([Int] -> [Int])
specops = fromList [("@", (0:))]

tryDecodeBinop :: String -> Maybe ([Int] -> [Int])
tryDecodeBinop s = fmap binop (binops !? s)

tryDecodeUnop :: String -> Maybe ([Int] -> [Int])
tryDecodeUnop s = fmap unop (unops !? s)

tryDecodeSpecop :: String -> Maybe ([Int] -> [Int])
tryDecodeSpecop s = specops !? s

decodeHelper :: String -> Maybe ([Int] -> [Int]) -> 
    (String -> Maybe([Int] -> [Int]))  -> Maybe ([Int] -> [Int])
decodeHelper s acc chooser 
    | isNothing acc = chooser s
    | otherwise     = acc 

decode :: String -> [Int] -> [Int]
decode s = maybe 
    (error "incorrect operation")
    id 
    (foldl 
        (decodeHelper s) Nothing
        [tryDecodeBinop, tryDecodeUnop, tryDecodeSpecop])

-- decode "+" = binop (+)
-- decode "-" = binop (-)
-- decode "*" = binop (*)
-- decode "/" = binop div
-- decode "%" = binop mod

-- decode "@" = (0:)

-- decode (c:[])
--   | '0' <= c && c <= '9' = unop f where f x = x*10 + (digitToInt c)

run = compose . (map decode) . reverse . words

