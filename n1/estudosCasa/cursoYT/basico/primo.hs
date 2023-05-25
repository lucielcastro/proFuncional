ehPrimo :: Int -> Bool
ehPrimo n = n > 1 && all (\x -> n `mod` x /= 0) [2..limite]
  where limite = floor . sqrt $ fromIntegral n

fibonacci :: Int -> Int
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)

primoDeFibonacci :: Int -> Int
primoDeFibonacci n = encontraPrimoDeFibonacci 1 n

encontraPrimoDeFibonacci :: Int -> Int -> Int
encontraPrimoDeFibonacci k n