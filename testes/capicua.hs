{-
Um número é capicua quando lido da esquerda para a direita ou da direita
para a esquerda representa sempre o mesmo valor, como por exemplo
77, 434, 6446, 82328.
Criar uma função para dizer se um numero é capicua
-}

-- funcao principal
capicua :: Int -> Bool
capicua c = (c == inverte c) -- verifica se o numero e o seu inverso é igual

-- funcao recursiva para expoente
expoente :: Int -> Int
expoente e
 | e == 0 = 0
 | otherwise = 1 + expoente (div e 10)

-- funcao recursisa que inverte o numero
inverte :: Int -> Int
inverte n
 | n == 0 = 0
 | otherwise = ((mod n 10) * (10 ^ expoente (div n 10))) + inverte (div n 10)