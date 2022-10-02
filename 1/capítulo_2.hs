----- FT 1



myFst :: (a, b) -> a

myFst (x, _) = x



----- FT 2



mySnd :: (a, b) -> b

mySnd (_, x) = x



----- FT 3



mySwap :: (b, a) -> (a, b)

mySwap (x, y) = (y, x)



----- FT 4



-- a )

distance2 :: Floating a => (a, a) -> (a, a) -> a

distance2 (x1, y1) (x2, y2) = sqrt((x1-x2)^2 + (y1-y2)^2)


-- b )

distanceInf :: (Num a, Ord a) => (a, a) -> (a, a) -> a

distanceInf (x1, y1) (x2, y2) = max(abs(x1-x2)) (abs(y1-y2))



----- FT 6



-- a )

myHead :: [a] -> a

myHead (h:_) = h


-- b )

hasLengthTwo :: [a] -> Bool

hasLengthTwo [_,_] = True
hasLengthTwo _ = False



----- FT 7



myLength :: Num b => [a] -> b

myLength [] = 0
myLength (_:xs) = 1 + myLength xs
