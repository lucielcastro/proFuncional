venda :: Int -> Int 
venda n
   | n == 0 = 5
   | n == 1 = 6
   | n == 2 = 4
   | n == 3 = 8
   | n == 4 = 2
   | n == 5 = 17
   | otherwise = 0

totaldevendas :: Int -> Int
totaldevendas n
   | n == 0 = venda 0
   | otherwise = totaldevendas (n-1) + venda n


totaldevendas2 :: Int -> Int
totaldevendas2 n
   | n == 0 = venda 0
   | n > 0 = venda n + totaldevendas2 (n-1)
   | otherwise = 0

maiorVenda :: Int -> Int
maiorVenda n
   | n == 0 = venda 0
   | maiorVenda (n-1) > venda n = maiorVenda(n-1)
   | otherwise = venda n 

maiorVenda2 :: Int -> Int
maiorVenda2 n
   |n == 0 = venda 0
   |otherwise = max (maiorVenda2 (n - 1)) (venda n)


{-▪Em que semana ocorreu a maior venda?
▪Existe alguma semana na qual nada foi vendido?
▪Em qual semana não houve vendas? (se é que houve
alguma)-}

{-▪putStr :: String -> IO()
▪show :: t -> String
▪read :: String -> t
▪Exemplo

putStr "IFMA\tCaxias"
putStr ("IFMA"++ "-" ++ "Caxias")
show (5+7)
show (True && False)
read "True" :: Bool
read "14" :: Int

▪Duas formas de representar cadeias de caracteres
▪Tipo String
▪ Utilizando aspas duplas
▪ Ex: “string”
▪Lista de caracteres
▪ Utilizando uma lista
▪ Ex: [‘s’,’t’,’r’,’i’,’n’,’g’]
▪Todas as funções aplicáveis a listas podem ser
utilizadas sobre strings


▪Haskell utiliza a ordem normal de avaliação
▪Leftermost-outertmost
▪Associado a um mecanismo preguiçoso
▪Avalia apenas quando necessário e apenas uma vez
▪Semelhante a avaliação curto-circuito tradicional
▪Exemplo

todosIguais::Int->Int->Int->Bool
todosIguais p s t = (p == s) && (s == t)

todosIguais (quadrado 3) valor (quadrado 2)
= ((quadrado 3) == valor) && (valor == (quadrado 2))
= ((3 * 3) == valor) && (valor == (quadrado 2))
= (9 == valor) && (valor == (quadrado 2))
= (9 == 39) && (39 == (quadrado 2))
= False && (39 == (quadrado 2))
= False

mdc::Int->Int->Int
mdc x y
| x == y = x
| x > y = mdc x-y y
| otherwise = mdc y x

produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
 | m == n = n
 | otherwise = m * produtoIntervalo (m + 1) n

div::Int->Int->Int
div x y
| x == y = 1
| x > y = 1 + div x-y y
| otherwise = 0

mmc::Int->Int->Int
mmc x y = div x*y (mdc x y)

somaQuadrados::Int->Int->Int
somaQuadrados n m = quadN + quadM
where
quadN = n * n
quadM = m * m

-}




