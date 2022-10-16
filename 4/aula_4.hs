
import Data.List

-- 4.1 -

data Arv a = Vazia | No a (Arv a) (Arv a) deriving Show

sumArv :: Num a => Arv a -> a

sumArv Vazia = 0
sumArv (No x esq dir) = sumArv esq + x + sumArv dir



-- 4.2 -

listar :: Arv a -> [a]
listar Vazia = []
listar (No x esq dir) = listar esq ++ [x] ++ listar dir

listardecrescente :: Ord a => Arv a -> [a]
listardecrescente arv = reverse(sort(listar arv))



-- 4.3 -

mytree = (No 11 (No 3 (No 2 Vazia Vazia) (No 5 Vazia Vazia)) (No 19 (No 13 Vazia Vazia) (No 23 Vazia Vazia)))

height Vazia = 0
height (No _ left right) = 1 + max (height left) (height right)

nivel :: Int -> Arv a -> [a]

nivel 0 (No x _ _) = [x]
nivel _ Vazia = []
nivel n (No x arv1 arv2) = nivel (n-1) arv1 ++ nivel (n-1) arv2



-- 4.4 -

mapArv :: (a -> b) -> Arv a -> Arv b

mapArv f (No x arv1 arv2) = No (f x) (mapArv f arv1) (mapArv f arv2)



-- 4.5 -
