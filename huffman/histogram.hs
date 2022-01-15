module ByteHistogram (byteHistogram) where

import qualified Data.ByteString as BS (ByteString, foldl)
import qualified Data.Map as M (Map, empty, insertWith)
import Data.Word (Word8)

byteHistogram :: BS.ByteString -> M.Map Word8 Int
byteHistogram = BS.foldl mUpdF M.empty
    where mUpdF m w = M.insertWith (+) w 1 m
