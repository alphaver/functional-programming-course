import qualified Huffman as Huf
import qualified ByteHistogram as BH
import qualified Data.Map as M 
import Data.Word (Word8, Word32)
import Data.Bits (FiniteBits, bit, (.&.), finiteBitSize, zeroBits)

import qualified Data.ByteString as BS
import System.Environment (getArgs)
import System.IO (withBinaryFile, Handle, IOMode(ReadMode, WriteMode))

fromBits :: (FiniteBits a, Integral a) => a -> [Word8] -> a
fromBits _ bs = foldl (\w b -> w*2 + (fromInteger $ toInteger b)) 0 bs

fromBits8 = fromBits (0 :: Word8)
fromBitsI = fromBits (0 :: Int)

toBits :: (FiniteBits a, Integral a) => a -> [Word8]
toBits w = 
    map (\i -> if w .&. (bit i) /= 0 then 1 else 0) [(l-1),(l-2)..0]
    where l = finiteBitSize w

toBits8 = toBits
toBitsI = toBits

packBits :: [Word8] -> [Word8]
packBits = f 
    where f [] = []
          f bs = if length bs >= 8
                 then (fromBits8 $ take 8 bs):(f $ drop 8 bs)
                 else error "the bits aren't byte-aligned"

unpackBits :: [Word8] -> [Word8]
unpackBits os = concat $ map toBits8 os

encodeHeaderH :: Word8 -> [Word8] -> M.Map Word8 Int -> [Word8]
encodeHeaderH w l m = if w == 255 then nl else encodeHeaderH (w+1) nl m
    where nl = l ++ (toBitsI $ M.findWithDefault 0 w m)

encodeHeader :: M.Map Word8 Int -> [Word8] 
encodeHeader = encodeHeaderH 0 []

decodeHeaderH :: Word8 -> M.Map Word8 Int -> [Word8] -> ([Word8], M.Map Word8 Int)
decodeHeaderH i m l = if i == 255 then (l', m') else decodeHeaderH (i+1) m' l'
    where m' = if n' == 0 then m else M.insert i n' m
          n' = fromBitsI $ take (finiteBitSize (0 :: Int)) l
          l' = drop (finiteBitSize (0 :: Int)) l

decodeHeader :: [Word8] -> ([Word8], M.Map Word8 Int)
decodeHeader = decodeHeaderH 0 M.empty

usage :: String
usage = "Usage: huffman [-d] INFILE OUTFILE\n" ++
        "    -d -- decode the INFILE and write the decoded file to OUTFILE;\n" ++ 
        "    if not provided, then vice versa"

parseArgs :: [String] -> IO (String, String, Bool)
parseArgs (flag:dec:enc:[]) = 
    if flag == "-d" then pure (dec, enc, True) else errorWithoutStackTrace usage
parseArgs (enc:dec:[])      = pure (enc, dec, False)
parseArgs _                 = errorWithoutStackTrace usage 

readHistogram :: Handle -> IO (M.Map Word8 Int)
readHistogram hFile = BS.hGetContents hFile >>= pure . BH.byteHistogram

encodeFile :: Huf.HTree Word8 -> Handle -> IO [Word8]
encodeFile root hFile = do
    bStr <- BS.hGetContents hFile
    let bStrW8 = BS.unpack bStr
    pure $ Huf.huffmanCode bStrW8 root

encode :: String -> String -> IO ()
encode inFile outFile = do
    histogram <- withBinaryFile inFile ReadMode readHistogram
    let hTree = Huf.huffmanTree histogram 
    fileCode <- withBinaryFile inFile ReadMode (encodeFile hTree)
    let hCode = (encodeHeader histogram) ++ 
                (toBitsI $ length fileCode) ++
                fileCode ++ 
                (take (8 - ((length fileCode) `mod` 8)) $ repeat 0)
    withBinaryFile outFile WriteMode (flip BS.hPut (BS.pack $ packBits hCode))

decode :: String -> String -> IO ()
decode inFile outFile = do
    bStr <- withBinaryFile inFile ReadMode BS.hGetContents
    let bStrBits = unpackBits $ BS.unpack bStr
    let (rest, header) = decodeHeader bStrBits
    let hTree = Huf.huffmanTree header
    let codeL = fromBitsI $ take (finiteBitSize (0 :: Int)) rest
    let rest' = drop (finiteBitSize (0 :: Int)) rest
    let hDecode = Huf.huffmanDecode (take codeL rest') hTree
    withBinaryFile outFile WriteMode (flip BS.hPut (BS.pack hDecode))

main :: IO ()
main = do
    args <- getArgs
    (inFile, outFile, doDecode) <- parseArgs args
    if doDecode
    then decode inFile outFile
    else encode inFile outFile
