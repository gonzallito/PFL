
{-
esta função calcula o dobro e o quadruplo de um número
-}
dobro x = 2*x
quadruplo x = dobro (dobro x)

-- calcular o factorial de um inteiro
factorial n = product [1..n]

-- calcular a média de dois valores
media x y = (x+y)/2
