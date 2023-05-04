
primoSophie :: Int -> Bool
primoSophie n
   | primo n && primo((2 * n) + 1) = True
   | otherwise = False

maiorSophie :: Int -> Int
maiorSophie n
   | n < 2 = -1
   | primoSophie n = n
   | otherwise = maiorSophie n-1
   
intprimoFib :: Int -> Int -> Int
intprimoFib n x
   | n<x = intprimoFib x n
   | n == x && primo(primoFib n) = 1
   | n == x && not(primo(primoFib n)) = 0
   | primo(fib x) = 1 + intprimoFib(primoFib n) (primoFib (x-1))
   | otherwise = intprimoFib(primoFib n) (primoFib(x-1))

contPrimo :: Int -> Int -> Int
contPrimo n1 n2
   | n1>n2 = contPrimo n2 n1
   | n1 == n2 && primo n2 = 1
   | n1 == n2 = 0
   | primo n1 && n1 /= n2 = 1 + contPrimo (n1+1) n2
   | otherwise = contPrimo (n1+1) n2

intPrimofib :: Int -> Int -> Int
intPrimofib n m
   | m<n = intPrimo m n
   | n == m && primo(fib n) = 1
   | n == m && not(primo (fib n)) = 0
   | primo( fib m) = 1 + intPrimofib n (fib m-1)
   | otherwise = intPrimofib n (fib m-1)

intPrimo :: Int -> Int -> Int
intPrimo n m 
   | m<n = intPrimo m n
   | n == m && primo n = 1
   | n == m && not(primo n) = 0
   | primo m = 1 + intPrimo n (m-1)
   | otherwise = intPrimo n (m-1)

primoFib :: Int -> Int
primoFib n = pFibAux n 1

pFibAux :: Int -> Int -> Int
pFibAux n x
   | primo(fib x) && n == 1 = fib x
   | primo(fib x) && n > 1 = pFibAux (n-1) (x+1)
   | otherwise = pFibAux n (x+1)

   
fib :: Int -> Int
fib n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fib(n-1) + fib(n-2)

primo :: Int -> Bool
primo n
   | n < 2     = False
   | otherwise = aux n (n-1)
     
aux :: Int -> Int -> Bool
aux n i
   | i == 1        = True
   | mod n i == 0  = False
   | otherwise     = aux n(i-1)