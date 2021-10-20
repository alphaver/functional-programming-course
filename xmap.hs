import Data.Maybe (fromJust)

data XMap k v = Nil | Node (k, v) (XMap k v) (XMap k v) deriving (Show) 

xNull :: XMap k v -> Bool
xNull Nil  = True
xNull _    = False

xInfixFold :: ((k, v) -> a -> a) -> XMap k v -> a -> a
xInfixFold _ Nil acc = acc
xInfixFold f (Node p l r) acc = xInfixFold f r $ f p $ xInfixFold f l acc

xSize :: XMap k v -> Int
xSize m = xInfixFold (\_ acc -> succ acc) m 0

xSingleton :: k -> v -> XMap k v
xSingleton k v = Node (k, v) Nil Nil

xFind :: (Ord k) => k -> XMap k v -> Maybe v
xFind _ Nil = Nothing
xFind sk (Node (rpk, rpv) l r) =
    case rpk `compare` sk of
        LT -> xFind sk r
        EQ -> Just rpv
        GT -> xFind sk l 

(!) :: (Ord k) => XMap k v -> k -> v
(!) m k = 
    case xFind k m of
        Nothing -> error "the key isn't present"
        Just v  -> v

(!?) :: (Ord k) => XMap k v -> k -> Maybe v
(!?) = flip xFind
 
xMin :: (Ord k) => XMap k v -> Maybe (k, v)
xMin Nil = Nothing
xMin (Node p Nil _) = Just p
xMin (Node _ l _) = xMin l

xMinRemove :: (Ord k) => XMap k v -> XMap k v
xMinRemove Nil = Nil
xMinRemove (Node _ Nil r) = r
xMinRemove (Node p l r) = Node p (xMinRemove l) r

xRootRemove :: (Ord k) => XMap k v -> XMap k v
xRootRemove Nil = Nil
xRootRemove (Node _ l Nil) = l
xRootRemove (Node _ Nil r) = r
xRootRemove (Node _ l r) =
    let rightMin = fromJust $ xMin r
    in Node rightMin l (xMinRemove r)

xAlter :: (Ord k) => (Maybe v -> Maybe v) -> k -> XMap k v -> XMap k v
xAlter f k Nil = maybe Nil (xSingleton k) (f Nothing)
xAlter f k n@(Node rp@(rpk, rpv) l r) =
    case rpk `compare` k of
        LT -> Node rp l (xAlter f k r)
        GT -> Node rp (xAlter f k l) r
        EQ -> maybe (xRootRemove n) (\v -> Node (k, v) l r) (f $ Just rpv) 

xInsert :: (Ord k) => k -> v -> XMap k v -> XMap k v
xInsert k v = xAlter (const $ Just v) k

xFromList :: (Ord k) => [(k, v)] -> XMap k v
xFromList = foldl f Nil 
    where f acc (k, v) = xInsert k v acc

xUpdate :: (Ord k) => (v -> Maybe v) -> k -> XMap k v -> XMap k v
xUpdate f k = xAlter g k
    where g Nothing     = Nothing
          g (Just v)    = f v 

xRemove :: (Ord k) => k -> XMap k v -> XMap k v
xRemove = xAlter $ const Nothing

xShow :: (Show k, Show v) => XMap k v -> String
xShow m = xInfixFold (\p acc -> acc ++ Prelude.show p ++ " ") m "" 
