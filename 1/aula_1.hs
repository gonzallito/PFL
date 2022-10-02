
-- 1.1 -

-- testaTriangulo :: Float -> Float -> Float -> Bool
testaTriangulo a b c =
  (a < b + c) && (b < a + c) && (c < a + b)



-- 1.2 -

-- areaTriangulo :: Float -> Float -> Float -> Float
areaTriangulo a b c =
  sqrt(s*(s-a)*(s-b)*(s-c))
  where s = (a+b+c)/2



-- 1.3 -

metades l = (lista1, lista2)
  where lista1 = take ((length l) `div` 2) l
        lista2 = drop ((length l) `div` 2) l



-- 1.4 -

--a)

last1 l = head (reverse l)

--b)

init1 l = take ((length l)-1) l



-- 1.5 -

-- binom :: Integer -> Integer -> Integer
binom n k = (product [1..n]) `div` (product [1..k]) * (product [1..(n-k)])

-- binom' :: Integer -> Integer -> Integer
binom' n k
  | (k < n - k) = product [(n-k+1)..n] `div` product [1..k]
  | otherwise = product [(k+1)..n] `div` product [1..(n-k)]



-- 1.6 -

-- raizes :: Float -> Float -> Float -> (Float, Float)
raizes a b c = (x1, x2)
  where x1 = (-b + delta) / (2 * a)
        x2 = (-b - delta) / (2 * a)
        delta = sqrt(b ^ 2 - 4 * a * c)



-- 1.7 -

{-
(a) [′a′, ′b′, ′c′] :: [Char]
(b) (′a′, ′b′, ′c′) :: (Char, Char, Char)
(c) [(False, ′0′), (True, ′1′)] :: [(Bool, Char)]
(d) ([False, True], [′0′, ′1′]) :: ([Bool], [Char])
(e) [tail , init, reverse] :: [Functions]
(f) [id , not] ::[Functions]
-}



-- 1.9 -

classifica :: Int -> String
classifica n
  | n >= 0 && n <= 9 = "reprovado"
  | n > 9 && n <= 12 = "suficiente"
  | n > 12 && n <= 15 = "bom"
  | n > 15 && n <= 18 = "muito bom"
  | n > 18 && n <= 20 = "muito bom com distincao"
  | otherwise = error "invalid grade (0 <= n <= 20)"



-- 1.10 -

classificaIMC :: Float -> Float -> String
classificaIMC weight height
  | imc < 18.5 = "baixo peso"
  | imc >= 18.5 && imc < 25 = "peso normal"
  | imc >= 25 && imc < 30 = "excesso de peso"
  | imc >= 30 = "obesidade"
  where imc = weight / (height^2)



-- 1.11 -

-- max, min :: Ord a => a -> a -> a
-- max x y = if x>=y then x else y
-- min x y = if x<=y then x else y


-- a )

max3 x y z
  | x >= y && x >= z = x
  | y >= x && y >= z = y
  | otherwise = z

min3 x y z
  | x <= y && x <= z = x
  | y <= x && y <= z = y
  | otherwise = z


-- b )

max3b x y z
  | max2 >= z = max2
  | otherwise = z
  where max2 = max x y

min3b x y z
  | min2 <= z = min2
  | otherwise = z
  where min2 = min x y



-- 1.12 -

xor :: Bool -> Bool -> Bool

xor a b = ((not a) && b) || (a && (not b))



-- 1.13 -

safetail :: [a] -> [a]

safetail [] = []
safetail (_:x) = x



-- 1.14 -

curtaa :: [a] -> Bool
-- curtab :: [a] -> Bool


-- a )


curtaa l = if (length l == 1) || (length l == 2) || (length l == 0) then True else False

{-
curtaa listaaa
  | lss == 0 || lss == 1 || lss == 2 = True
  | otherwise = False
    where lss = length listaaa
-}


-- b )


curtab [] = True
curtab [_] = True
curtab [_,_] = True
curtab _ = False



-- 1.15 -


-- a )


medianaa x y z
  | x <= y && x >= z = x
  | x >= y && x <= z = x
  | y <= x && y >= z = y
  | y >= x && y <= z = y
  | otherwise = z


-- b )


medianab x y z = sum [x, y, z] - maximum [x, y, z] - minimum [x, y, z]



-- 1.16 -


unidades = ["zero", "um", "dois", "tres", "quatro", "cinco", "seis", "sete", "oito", "nove"]

dez_a_dezanove = ["dez", "onze", "doze", "treze", "catorze", "quinze", "dezasseis", "dezassete", "dezoito", "dezanove"]

dezenas = ["vinte", "trinta", "quarenta", "cinquenta", "sessenta", "setenta", "oitenta", "noventa"]

centenas = ["cento", "duzentos", "trezentos", "quatrocentos", "quinhentos", "seiscentos", "setecentos", "oitocentos", "novecentos"]

divide2 :: Int -> (Int, Int)
divide2 n = (n`div`10, n`mod`10)

combina2 :: (Int, Int) -> String
combina2 (0, u) = unidades !! u
combina2 (1, u) = dez_a_dezanove !! u
combina2 (d, 0) = dezenas !! (d-2)
combina2 (d, u) = dezenas !! (d-2) ++ " e " ++ unidades !! u

converte2 :: Int -> String
converte2 n | n < 100 = combina2 (divide2 n)

divide3 :: Int -> (Int, Int)
divide3 n = (n`div`100, n`mod`100)

combina3 :: (Int, Int) -> String
combina3 (0, n) = converte2 n
combina3 (1, 0) = "cem"
combina3 (c, 0) = centenas !! (c-1)
combina3 (c, n) = centenas !! (c-1) ++ " e " ++ converte2 n

converte3 :: Int -> String
converte3 n | n < 1000 = combina3 (divide3 n)

divide6 n = (n `div` 1000, n `mod` 1000)

combina6 (0, n) = converte3 n
combina6 (1, 0) = "mil"
combina6 (1, n) = "mil" ++ ligar n ++ converte3 n
combina6 (m, 0) = converte3 m ++ " mil"
combina6 (m, n) = converte3 m ++ " mil" ++ ligar n ++ converte3 n

converte6 :: Int -> String
converte6 n | n < 1000000 = combina6 (divide6 n)

ligar :: Int -> String
ligar r
  | r < 100 || r `mod` 100 == 0 = " e "
  | otherwise                   = " "

converte :: Int -> String
converte = converte6
