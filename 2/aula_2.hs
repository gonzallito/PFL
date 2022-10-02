
import Data.Char (chr, ord)
import Data.Char



-- 2.1 -

-- a )

myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = if (x == True) then myand xs else False


-- b )

myor :: [Bool] -> Bool
myor [] = True
myor (x:xs) = if (x == False) then myor xs else True


-- c )

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

-- myconcat listas = [valor | lista<-listas, valor<-lista]


-- d )

myreplicate :: Int -> a -> [a]
myreplicate 0 a = [];
myreplicate n a = [a] ++ myreplicate (n-1) a


-- e )

nesimo :: [a] -> Int -> a
nesimo (x:xs) 0 = x
nesimo [] n = error "index too large"
nesimo (x:xs) a = nesimo xs (a-1)

-- f )

myelem :: Eq a => a -> [a] -> Bool
myelem n [] = False
myelem n (x:xs)
  | n == x = True
  | otherwise = myelem n xs



-- 2.2 -

interperse :: a -> [a] -> [a]
interperse s [] = error "Empty String"
interperse s [a] = [a]
interperse s (x:xs) = [x] ++ [s] ++ interperse s xs



-- 2.3 -

mdc a b
  | b == 0 = a
  | otherwise = mdc b (a`mod`b)



-- 2.4 -

-- a )

myinsert :: Ord a => a -> [a] -> [a]
myinsert n [] = [n]
myinsert n (x:xs)
  | n <= x = n:x:xs
  | otherwise = x:(myinsert n xs)


-- b )

myisort [] = []
myisort (x:xs) = myinsert x (myisort xs)



-- 2.5 -

-- a )

myminimum :: Ord a => [a] -> a
myminimum [x] = x
myminimum (x:y:xs) = if x <= y then myminimum(x:xs) else myminimum (y:xs)


-- b )

mydelete :: Eq a => a -> [a] -> [a]
mydelete x [] = []
mydelete n (x:xs)
  | n == x = xs
  | otherwise = x : mydelete n xs


-- c )

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort l = myminimum l : (ssort (mydelete (myminimum l) l))



-- 2.6 -

soma100 = sum [x^2 | x<-[1..100]]



-- 2.7 -

-- a )

aprox :: Int -> Double

aprox x = 4 * sum (take x [((-1)**n) / ((2*n) + 1) | n<-[0..]])


-- b )

aproxb :: Int -> Double

aproxb x = sqrt( 12 * sum (take x [((-1)**n) / (n + 1)**2 | n<-[0..]]))



-- 2.8 -

dotprod :: [Float] -> [Float] -> Float

dotprod x y = sum [ a*b | (a,b) <- zip x y ]



-- 2.9 -

divprop :: Integer -> [Integer]
-- divprop x = [x`mod`n == 0 | n <- [1..(x-1)]]
divprop x = [ n | n <- [1..(x-1)], x`mod`n == 0]



-- 2.10 -

perfeitos :: Integer -> [Integer]

perfeitos x = [ n | n <- [1..x], sum (divprop n) == n]



-- 2.11 -

pitagoricos :: Integer -> [(Integer , Integer , Integer)]

pitagoricos a = [(x, y, z) | x <- [1..a], y <- [1..a], z <- [1..a], x^2 + y^2 == z^2]



-- 2.12 -

primo :: Integer -> Bool

primo x = length (divprop x) == 1



-- 2.13 -

mersennes :: [Int]

mersennes = [ 2^n - 1 | n <- [0..30], primo (2^n - 1)]



-- 2.14 -

binom :: Int -> Int -> Int

binom n k = (product [1..n]) `div` (product [1..k]) * (product [1..(n-k)])


pascal :: Int -> [[Int]]

pascal n =  [[ binom i j | j <- [0..i]] | i <- [0..n]]



-- 2.15 -

letraInt :: Char -> Int
letraInt c = ord c - ord 'A'

intLetra :: Int -> Char
intLetra n = chr (n + ord 'A')

maiuscula :: Char -> Bool
maiuscula x = x >='A' && x <= 'Z'

desloca :: Int -> Char -> Char
desloca k x
  | maiuscula x = intLetra ((letraInt x + k) `mod` 26)
  | otherwise = x

cifrar :: Int -> String -> String
cifrar k xs = [desloca k x | x <- xs]



-- 2.16 -

concat1 :: [[a]] -> [a]
concat1 a = [ x | b <- a, x <- b ]

replicate2 :: Int -> a -> [a]
replicate2 a b = [ b | x <- [1..a] ]

twoexclamations :: [a] -> Int -> a
twoexclamations a b = head [ x | (x,y) <- zip a [0..b], y == b]



-- 2.17 -

forte :: String -> Bool

forte x = (length x >= 8) && letraMaiuscula && letraMiniscula && algarismo
  where
    letraMaiuscula = peloMenosUm isUpper x
    letraMiniscula = peloMenosUm isLower x
    algarismo = peloMenosUm isDigit x
    peloMenosUm a b = length [ y | y <- b, a y] >= 1



-- 2.18 -

-- a )

mindiv :: Int -> Int

mindiv x
  | null a = x
  | otherwise = head a
  where a = [ n | n <- [2..floor (sqrt (fromIntegral x))], x`mod`n == 0]


-- b )

bestIsPrime x = x > 1 && mindiv x == x
