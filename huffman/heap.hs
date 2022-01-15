module Heap 
(MinHeap, empty, singleton, size, insert, getMin, delMin) 
where

data MinHeap a = Nil | Node Int a (MinHeap a) (MinHeap a) deriving (Show)

empty :: MinHeap a
empty = Nil

singleton :: a -> MinHeap a
singleton e = Node 1 e Nil Nil

subtreeSize :: MinHeap a -> Int
subtreeSize Nil = 0
subtreeSize (Node nd _ _ _) = nd

size :: MinHeap a -> Int
size = subtreeSize

makeT :: (Ord a) => a -> MinHeap a -> MinHeap a -> MinHeap a
makeT e l r = 
    if (subtreeSize l) >= (subtreeSize r)
    then Node (subtreeSize l + subtreeSize r + 1) e l r
    else Node (subtreeSize l + subtreeSize r + 1) e r l 

merge :: (Ord a) => MinHeap a -> MinHeap a -> MinHeap a
merge Nil h = h
merge h Nil = h
merge h1@(Node _ hv1 hl1 hr1) h2@(Node _ hv2 hl2 hr2) =
    if hv1 <= hv2
    then makeT hv1 hl1 (merge hr1 h2)
    else makeT hv2 hl2 (merge h1 hr2)

insert :: (Ord a) => a -> MinHeap a -> MinHeap a
insert e = merge $ singleton e

getMin :: MinHeap a -> a
getMin Nil = error "empty MinHeap"
getMin (Node _ e _ _) = e

delMin :: (Ord a) => MinHeap a -> MinHeap a
delMin Nil = error "empty MinHeap"
delMin (Node _ _ h1 h2) = merge h1 h2
