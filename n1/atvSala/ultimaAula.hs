
multPrimo :: Int -> Int
multPrimo n
   | n < 10 && primo n = n
   | n < 10 && not(primo n) = 1
   | primo(mod n 10) = mod n 10 * multPrimo(div n 10)
   | otherwise = multPrimo(div n 10)

somaDigPar :: Int -> Int
somaDigPar n
   | n < 10 && ePar n = n
   | n < 10 && not(ePar n) = 0
   | ePar(mod n 10) = mod n 10 + somaDigPar(div n 10)
   | otherwise = somaDigPar(div n 10)
      --where
         --resto = mod n 10
         --quo div n 10

somaDigI :: Int -> Int
somaDigI n
   | n < 10 && ePar n = 0
   | n < 10 && impar n = 1
   | ePar(mod n 10) = 1 + somaDigI(div n 10)
   | otherwise = somaDigI(div n 10)

ePar :: Int -> Bool
ePar n
   | mod n 2 == 0 = True
   | otherwise = False
   
impar :: Int -> Bool
impar n = ( (mod n 2) /= 0)


primo :: Int -> Bool
primo n
  | n < 2     = False
  | otherwise = aux n (n-1)
  
aux :: Integral t => t -> t -> Bool
aux n i
  | i == 1        = True
  | mod n i == 0  = False
  | otherwise     = aux n(i-1)

{--goldbach :: Int -> Int -> Int
goldbach n
  | n 
  --}
