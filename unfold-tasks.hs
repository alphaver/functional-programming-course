import Data.List

xUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
xUnfoldr f state = maybe [] (\(x, state') -> x : (xUnfoldr f state')) (f state)

xUnfoldlHelper :: [a] -> (b -> Maybe (a, b)) -> b -> [a]
xUnfoldlHelper lAcc f state =
    case f state of Nothing -> lAcc
                    Just (x, state') -> xUnfoldlHelper (x:lAcc) f state' 

xUnfoldl :: (b -> Maybe (a, b)) -> b -> [a]
xUnfoldl = xUnfoldlHelper []

iota :: (Integral a) => a -> [a]
iota 0 = []
iota n =
    let nPred = pred n 
    in nPred : (iota nPred)

newIotaHelper :: (Integral a) => a -> a -> [a]
newIotaHelper count bound = 
    if count == bound 
    then []
    else count : (newIotaHelper (succ count) bound) 

newIota :: (Integral a) => a -> [a]
newIota = newIotaHelper 0

unfoldIota :: (Integral a) => a -> [a]
unfoldIota bound = unfoldr unfoldIotaGen 0
    where unfoldIotaGen x = if x == bound then Nothing else Just (x, x + 1)

recDigitsHelper :: (Integral a) => [a] -> a -> a -> [a]
recDigitsHelper lAcc 0 _ = lAcc
recDigitsHelper lAcc n base = 
    let digit = n `mod` base
        remainder = n `div` base
    in recDigitsHelper (digit:lAcc) remainder base

recDigits :: (Integral a) => a -> a -> [a]
recDigits 0 _ = [0]
recDigits n base = recDigitsHelper [] n base

unfoldDigits :: (Integral a) => a -> a -> [a]
unfoldDigits 0 _ = [0]
unfoldDigits x base = xUnfoldl digit x
    where digit n = if n == 0 then Nothing else Just (n `mod` base, n `div` base)

foldDigits :: (Integral a) => [a] -> a -> a
foldDigits lst base = foldl (\acc d -> acc * base + d) 0 lst

divisorFreeNumber :: (Integral a) => a -> a -> a
divisorFreeNumber 0 _ = 0
divisorFreeNumber n divisor
    | n `mod` divisor == 0  = divisorFreeNumber (n `div` divisor) divisor
    | otherwise             = n

recPDONHelper :: (Integral a) => a -> a -> [a]
recPDONHelper _ 1 = []
recPDONHelper divisor n
    | divisor * divisor > n = [n]
    | n `mod` divisor == 0  = divisor : (recPDONHelper (divisor+1) (divisorFreeNumber n divisor))
    | otherwise             = recPDONHelper (divisor+1) n

recPrimeDivisorsOfNumber :: (Integral a) => a -> [a]
recPrimeDivisorsOfNumber 1 = [] 
recPrimeDivisorsOfNumber n = recPDONHelper 2 n

unfoldPrimeDivisorsHelper :: (Integral a) => (a, a) -> Maybe (a, (a, a))
unfoldPrimeDivisorsHelper (divisor, n) 
    | n == 1                = Nothing
    | divisor * divisor > n = Just (n, (divisor, 1)) 
    | n `mod` divisor == 0  = Just (divisor, (divisor + 1, divisorFreeNumber n divisor))
    | otherwise             = unfoldPrimeDivisorsHelper (divisor + 1, n)

unfoldPrimeDivisorsOfNumber :: (Integral a) => a -> [a]
unfoldPrimeDivisorsOfNumber n = unfoldr unfoldPrimeDivisorsHelper (2, n)

infFib :: [Integer]
infFib = unfoldr helper (0, 1)
    where helper (curr, next) = Just (curr, (next, curr + next))

infPrimes :: [Integer]
infPrimes = eratosthenes [2..]
    where eratosthenes (x:xs) = x : (eratosthenes $ filter (\y -> y `mod` x /= 0) xs)
