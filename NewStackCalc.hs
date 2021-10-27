import Control.Monad (join)
import Data.List (find)
import Data.Map (Map, fromList, (!?), (!), insert, empty)
import Data.Maybe (isJust, fromJust)
import Text.Read (readMaybe)
import Data.Char (isDigit, digitToInt)

compose [] = id
compose (f:fs) = f . (compose fs)

type Calc a = ([a], Map String a)
type ExecState a = (Calc a, [String], [String])
type StackOp a = [a] -> [a]
type CalcOp a = Calc a -> Calc a
type ExecOp a = ExecState a -> ExecState a
type Decoder a = String -> Maybe (ExecOp a)

combineDecoders :: [Decoder a] -> Decoder a
combineDecoders ds s = join $ find isJust $ map ($s) ds

-- run :: Decoder a -> String -> ExecOp a
-- run decode = compose . (map $ fromJust . decode) . reverse . words

run :: Decoder a -> ExecOp a
run _ est@(_, _, []) = est
run decode est@(_, _, r:rs) = run decode $ (fromJust $ decode r) est

compileProg :: String -> ExecState a
compileProg prog = (([], empty), [], words prog)

stackToCalcOp :: StackOp a -> CalcOp a
stackToCalcOp op (st, mem) = (op st, mem)

calcToExecOp :: CalcOp a -> ExecOp a
calcToExecOp op (c, p, (r:rs)) = (op c, (r:p), rs)

stackToExecOp = calcToExecOp . stackToCalcOp

binop :: (a -> a -> a) -> StackOp a
binop op (x: (y: zs)) = (op y x) : zs

unop :: (a -> a) -> StackOp a
unop op (x: ys) = (op x) : ys

----

binops :: Map String (Int -> Int -> Int)
binops = fromList [
    ("+", (+)),
    ("-", (-)),
    ("*", (*)),
    ("/", (div)),
    ("%", (mod))
  ]

tryDecodeBinOp :: Decoder Int
tryDecodeBinOp s = fmap stackToExecOp $ fmap binop $ binops !? s

unops :: Map String (Int -> Int)
unops = fromList [
    ("~", negate),
    ("INC", (+1)),
    ("DEC", subtract 1)
  ]

tryDecodeUnOp :: Decoder Int
tryDecodeUnOp s = fmap stackToExecOp $ fmap unop $ unops !? s

tryDecodeLiteral :: Decoder Int
tryDecodeLiteral s = fmap stackToExecOp $ fmap (:) $ readMaybe s

varname ::  String -> Maybe String
varname ""       = Nothing
varname (')':[]) = Just ""
varname (c:rs)   = fmap (c:) (varname rs)

tryDecodeLoader :: Decoder a
tryDecodeLoader ('L':'O':'A':'D':'(':ss) = fmap loader (varname ss)
    where loader var = calcToExecOp (\(st, mem) -> ((mem ! var):st, mem))
tryDecodeLoader _ = Nothing

tryDecodeStorer :: Decoder a
tryDecodeStorer ('S':'T':'O':'R':'E':'(':ss) = fmap storer (varname ss) 
    where storer var = calcToExecOp (\(st@(x:_), mem) -> (st, insert var x mem))
tryDecodeStorer _ = Nothing

specials :: Map String (CalcOp a)
specials = fromList [
    ("RESET", const ([], empty)),
    ("DISCARD", stackToCalcOp tail),
    ("SWAP", stackToCalcOp (\(x: (y: zs)) -> y: x: zs)),
    ("DUPLICATE", stackToCalcOp (\(x: ys) -> x: x: ys))
  ]

tryDecodeSpecial :: Decoder Int
tryDecodeSpecial = (fmap calcToExecOp) . (specials !?)

indirmanips :: (Show a) => Map String (CalcOp a)
indirmanips = fromList [
    ("ILOAD",  (\((x:ys), mem) -> ((mem ! (show x)):ys, mem))),
    ("ISTORE", (\(st@(x:y:_), mem) -> (st, (insert (show x) y mem))))
  ]

tryDecodeIndir :: Decoder Int
tryDecodeIndir = (fmap calcToExecOp) . (indirmanips !?)

intToParenthesisHelper :: Int -> String -> Maybe Int
intToParenthesisHelper a (')':[]) = Just a
intToParenthesisHelper _ []       = Nothing 
intToParenthesisHelper a (d:ss)  
    | isDigit d = intToParenthesisHelper (a * 10 + digitToInt d) ss
    | otherwise = Nothing 

jumper :: Int -> ExecOp a
jumper 0 est = est
jumper n (_, _, []) | n > 0 = error "can't jump out of program"
jumper n (_, [], _) | n < 0 = error "can't jump out of program"
jumper n (cst, pcs@(p:ps), rcs@(r:rs)) 
    | n > 0 = jumper (pred n) (cst, (r:pcs), rs)
    | n < 0 = jumper (succ n) (cst, ps, (p:rcs))

tryDecodeJump :: Decoder a
tryDecodeJump ('J':'M':'P':'(':ss) = fmap jumper (intToParenthesisHelper 0 ss)

intStackCalcDecoders :: [Decoder Int]
intStackCalcDecoders = [
    tryDecodeLoader,
    tryDecodeStorer,
    tryDecodeIndir,
    tryDecodeSpecial,
    tryDecodeLiteral,
    tryDecodeBinOp,
    tryDecodeUnOp,
    tryDecodeJump
  ]

intStackCalcSemantics = combineDecoders intStackCalcDecoders

