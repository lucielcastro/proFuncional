
fibPrimo :: Int -> Int
fibPrimo n = contFib 1 1
  where
    contFib :: Int -> Int -> Int
    contFib cont n
      | cont == n && primo(fib n) = fib n
      | primo(fib n) = contFib (cont+1) (n+1)
      | otherwise = contFib cont (n+1)
      
primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Integral t => t -> t -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

fib :: Int -> Int
fib n
   | n == 0 = 0
   | n == 1 = 1
   | otherwise = fib(n-1) + fib(n-2)


  
  