
l :: [Integer]
l=[1,2,3,4]

r :: [Integer]
r=[2*a | a <- l]

t :: [Integer]
t=[a | a <- l, mod a 2 == 0]

z :: [Integer]
z = [2*a | a <- [1,2,3,4], mod a 2 == 0]


somaPares :: [(Int, Int)] -> [Int]
somaPares pares = [a + b | (a, b) <- pares]

quickSort :: [Int] -> [Int]
quickSort [] = []
quickSort ( a : x) = quickSort [ y | y <- x, y <= a] ++ [a] ++ quickSort [ y | y <- x, y > a]

listaDiv :: Int -> Int -> [Int]
listaDiv n d
   | d == 1 = [1]
   | mod n d == 0 = listaDiv n (d-1) ++ [d]
   | otherwise = listaDiv n (d-1)

divisores :: Int -> [Int]
divisores n = [ a | a <- [1.. (n-1)], mod n a == 0 ]

perfeito :: Int -> Bool
perfeito n = sum(divisores n) == n

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1.. n], perfeito x]

concatena :: [[Int]] -> [Int]
concatena lista = [x | sub <- lista, x <- sub]

--fib zf
fibList :: Int -> [Int]
fibList x = [n | n <- [1 .. x], pertenceFib n]

fib :: Int -> Int
fib n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fib(n-1) + fib(n-2)

pertenceFib :: Int -> Bool
pertenceFib n = auxpertenceFib n 1 1

auxpertenceFib::Int->Int->Int->Bool
auxpertenceFib n cont compara
   |compara == n = True
   |compara > n = False
   |otherwise = auxpertenceFib n (cont+1) (fib cont)
