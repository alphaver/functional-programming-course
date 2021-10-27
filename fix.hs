oneGen :: [Int] -> [Int]
oneGen l = 1:l

powersOfTwoGen :: [Int] -> [Int]
powersOfTwoGen l = 1:nl
    where nl = map (2*) l
