
import Data.List

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
