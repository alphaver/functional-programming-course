module Huffman
(HTree, huffmanTree, huffmanCode, huffmanDecode)
where

import qualified Heap as H
import qualified Data.Map as M
import Data.Word (Word8)
import Data.Map ((!))

data HTree a = HTLeaf Int a | HTINode Int (HTree a) (HTree a)

instance Ord (HTree a) where
    (HTLeaf f1 _) `compare` (HTINode f2 _ _) = f1 `compare` f2
    (HTINode f1 _ _) `compare` (HTLeaf f2 _) = f1 `compare` f2
    (HTINode f1 _ _) `compare` (HTINode f2 _ _) = f1 `compare` f2
    (HTLeaf f1 _) `compare` (HTLeaf f2 _) = f1 `compare` f2

instance Eq (HTree a) where
    n1 == n2 = n1 `compare` n2 == EQ

singleton :: a -> Int -> HTree a
singleton = flip HTLeaf

mergeNodes :: HTree a -> HTree a -> HTree a
mergeNodes n1@(HTLeaf f1 _) n2@(HTLeaf f2 _) = HTINode (f1+f2) n1 n2 
mergeNodes n1@(HTINode f1 _ _) n2@(HTINode f2 _ _) = HTINode (f1+f2) n1 n2
mergeNodes n1@(HTLeaf f1 _) n2@(HTINode f2 _ _) = HTINode (f1+f2) n1 n2 
mergeNodes n1@(HTINode f1 _ _) n2@(HTLeaf f2 _) = HTINode (f1+f2) n1 n2 

mergeNodesAndUpdate :: H.MinHeap (HTree a) -> H.MinHeap (HTree a)
mergeNodesAndUpdate h =
    let n1  = H.getMin h
        h'  = H.delMin h
        n2  = H.getMin h'
        h'' = H.delMin h'
    in H.insert (mergeNodes n1 n2) h''

buildHTree :: H.MinHeap (HTree a) -> HTree a
buildHTree h
    | H.size h == 0 = error "no leaves to form the tree from"
    | H.size h == 1 = H.getMin h
    | otherwise     = buildHTree $ mergeNodesAndUpdate h

huffmanTree :: M.Map a Int -> HTree a
huffmanTree m = buildHTree $ M.foldlWithKey (\h k v -> H.insert (singleton k v) h) H.empty m

huffmanCodeTH :: (Ord a) => [Word8] -> M.Map a [Word8] -> HTree a -> M.Map a [Word8]
huffmanCodeTH c m (HTLeaf _ v) = M.insert v c m
huffmanCodeTH c m (HTINode _ l r) =
    let m' = huffmanCodeTH (c ++ [0]) m  l
    in huffmanCodeTH (c ++ [1]) m' r

huffmanCodeTable :: (Ord a) => HTree a -> M.Map a [Word8]
huffmanCodeTable = huffmanCodeTH [] M.empty

huffmanCode :: (Ord a) => [a] -> HTree a -> [Word8]
huffmanCode as r = 
    let t = huffmanCodeTable r
    in concat $ map (t !) as

consumeBits :: [Word8] -> HTree a -> ([Word8], a)
consumeBits []     (HTINode _ _ _) = error "incorrect Huffman code"
consumeBits bs     (HTLeaf _ x)    = (bs, x)
consumeBits (b:bs) (HTINode _ l r) = consumeBits bs (if b == 0 then l else r)

huffmanDecodeH :: [a] -> [Word8] -> HTree a -> [a]
huffmanDecodeH res [] _ = res
huffmanDecodeH res bits r = 
    let (bits', nA) = consumeBits bits r
    in huffmanDecodeH (res ++ [nA]) bits' r

huffmanDecode :: [Word8] -> HTree a -> [a]
huffmanDecode = huffmanDecodeH [] 
