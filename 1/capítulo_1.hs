----- IN 5



-- double :: Num a => a -> a
double x = 2*x

-- nand :: Bool -> Bool -> Bool
nand x y = not(x && y)

-- funcX :: Floating a => a -> a -> a -> a -> a
-- funcX(x, a, b, c) = aT^2 + bT + c, where T = cos(x) + sin(x)
funcX a b c x =
  a*t^2 + b*t + c
  where t = cos x + sin x



----- IN 6



-- half :: Fractional a => a -> a
half x = x / 2

-- xor :: bool -> bool -> Bool
xor a b = ((not a) && b) || (a && (not b))

-- cbrt :: Floating a => a -> a
cbrt x = x**(1/3)

-- heron :: Floating => a -> a -> a -> a
heron a b c =
  sqrt(s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2



----- IN 7



-- isTriangular :: (Ord a, Num a) => a -> a -> a -> Bool
isTriangular a b c =
  (a <= b + c) && (b <= a + c) && (c <= a + b)

-- isPythagorean :: (Num a, Eq a) => a -> a -> a -> Bool
isPythagorean a b c =
  (a^2 == b^2 + c^2) || (b^2 == a^2 + c^2) || (c^2 == a^2 + b^2)



----- IN 8



-- five :: (Eq a, Num a) => a -> [Char]
five n = if n == 5 then "five" else "not_five"



----- IN 9



-- min3 :: Ord a => a -> a -> a -> a
min3 x y z
  | x < y && x < z = x
  | y < x && y < z = y
  | otherwise = z



----- IN 10



-- testPh :: (Ord a, Floating a) => a -> [Char]
testPh x
  | ph == 7 = "neutral"
  | ph > 7 = "basic"
  | ph < 7 = "acid"
  where ph = -logBase 10 x



----- IN 11



-- max3 :: Ord a => a -> a -> a -> a
max3 x y z
  | x > y && x > z = x
  | y > x && y > z = y
  | otherwise = z



----- IN 12



-- testBMI :: (Ord a, Fractional a) => a -> a -> [Char]
testBMI weight height
  | bmi < 18.5 = "underweight"
  | bmi >= 18.5 && bmi < 25.0 = "healthy weight"
  | bmi >= 25.0 && bmi < 30.0 = "overweight"
  | bmi >= 30.0 = "obese"
  where bmi = weight / (height^2)



----- IN 14



-- factorial :: (Ord p, Num p) => p -> p
factorial 0 = 1
factorial n
  | n > 0 = n * factorial (n - 1)
  | n < 0 = error "invalid_argument (num > 0)"



----- IN 15



-- myGcd :: Integral a => a -> a -> a
{-
myGcd a b
  | b == 0 = a
  | b > 0 = myGcd b (`mod` a b)
-}



----- IN 16



-- mPower :: ( Fractional a, Integral t) => a -> t -> a
mPower a 0 = 1
mPower a b = if b > 0 then (mPower a (b-1)) * a else (mPower a (b+1)) / a



----- IN 17



-- fib :: (Num a, Ord a, Num p) => a -> p
fib 0 = 0
fib 1 = 1
fib a
  | a > 0 = fib (a-1) + fib (a-2)
  | otherwise = error "invalid_argument (num >= 0)"



----- IN 18



-- ackermann :: (Num a, Ord a, Num t, Ord t) => a -> t -> t
ackermann m n
  | m == 0 = n + 1
  | m > 0 && n == 0 = ackermann (m - 1) 1
  | m > 0 && n > 0 = ackermann (m - 1) (ackermann m (n - 1))



----- IN 19



-- pascal :: (Num a, Ord a, Num p) => a -> a -> p
pascal 1 _ = 1
pascal k n
  | k > 1 && k == n = 1
  | k > 1 && n > k = pascal (k -1) (n -1) + pascal k (n -1)
  | k > 1 && n < k = error "n must be greater or equal than k"
  | otherwise = error "k must be greater than 0"



----- IN 20



-- isPrime :: Integral t => t -> Bool
isPrime n
  | n <= 1 = False
  | n == 2 = True
  | n > 0 = if length (divisors n) == 2 then True else False
    where divisors n = [x | x <- [1..n], n `mod` x == 0]
