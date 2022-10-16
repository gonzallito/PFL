
import Data.List
import Data.List.Split

-- 3.1 -

primeiro f p x = map f (filter p x)



-- 3.2 -

dec2int :: [Int] -> Int

dec2int = foldl (\res n -> 10 * res + n) 0



-- 3.3 -

zipWith1 :: (a -> b -> c) -> [a] -> [b] -> [c]

zipWith1 f [] _ = []
zipWith1 f _ [] = []
zipWith1 f (x:xs) (y:ys) = (f x y):(zipWith1 f xs ys)
-- zipWith1 f x y = [f a b | (a,b) <- zip x y]



-- 3.4 -

isort :: Ord a => [a] -> [a]

isort x = foldr insert [] x
-- isort = foldr insert []



-- 3.5 -

-- a )

maximum1 :: Ord a => [a] -> a

maximum1 = foldl1 max
-- tambem dá foldr1

minimum1 :: Ord a => [a] -> a

minimum1 = foldr1 min
-- tambem dá foldl1



-- b )

myFoldl1 f x = foldl f (head x) (tail x)

myFoldr1 f x = foldr f (last x) (init x)



-- 3.6 -

mdc a b = fst (until (\(a,b) -> b == 0) (\(a,b) -> (b,a`mod`b)) (a,b))



-- 3.7 -

-- a )

(+++) :: [a] -> [a] -> [a]

x +++ y = foldr (:) y x



-- b )

concatFoldr :: [[a]] -> [a]

concatFoldr = foldr (++) []



-- c )

reverseFoldr :: [a] -> [a]

reverseFoldr = foldr (\x y -> y ++ [x]) []



-- d )

reverseFoldl :: [a] -> [a]

reverseFoldl = foldl (\x y -> y:x) []



-- e )

elemAny :: Eq a => a -> [a] -> Bool

elemAny x y = any (x==) y



-- 3.8 -

palavras :: String → [String]

palavras [] = []
palavras [x] = [x]
palavras (x:xs)
  | x == ' ' = palavras ++ [x]
  | otherwise
