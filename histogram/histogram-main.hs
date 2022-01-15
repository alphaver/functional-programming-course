import ByteHistogram (byteHistogram)

import qualified Data.ByteString as BS (ByteString, hGetContents)
import qualified Data.Map as M (Map, findWithDefault, (!), foldrWithKey)
import Data.Word (Word8)

import System.Environment (getArgs)
import System.IO (withBinaryFile, Handle, IOMode(ReadMode))
import Text.Printf (printf)

readHistogram :: Handle -> IO (M.Map Word8 Int)
readHistogram hFile = BS.hGetContents hFile >>= pure . byteHistogram

checkArgs :: [String] -> IO ()
checkArgs (_:[]) = pure ()
checkArgs _ = errorWithoutStackTrace "must provide only the name of the file"

prettyHistogram :: M.Map Word8 Int -> String
prettyHistogram m = form 0 ""
    where form 256 str = str
          form i str   = form (i+1) (str ++ (newStr . fromInteger) i)
          newStr i     = printf "%02x: %d\n" i $ M.findWithDefault 0 i m

prettyHistogram' :: M.Map Word8 Int -> String
prettyHistogram' = M.foldrWithKey newStr ""
    where newStr i j str = (printf "%02x: %d\n" i j) ++ str

main :: IO ()
main = do
    args <- getArgs
    checkArgs args
    histogram <- withBinaryFile (head args) ReadMode readHistogram
    putStr $ prettyHistogram' histogram 
