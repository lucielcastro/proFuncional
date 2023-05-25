contarDigitos :: Int -> Int -> Int
contarDigitos n k
 | n == 0 = 0
 | (mod n 10) == k = 1 + contarDigitos (div n 10) k
 | otherwise = contarDigitos (div n 10) k

superFatorial :: Int -> Int
superFatorial n
  | n == 0 = 1
  | otherwise = (fatorial n) * (superFatorial (n - 1))

produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
   | m == n = n
   | otherwise = m * produtoIntervalo (m + 1) n

produtoIntervalo :: Int -> Int -> Int
produtoIntervalo m n
    | m == n = n
    | otherwise = m * produtoIntervalo (m + 1) n

