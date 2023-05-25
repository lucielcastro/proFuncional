
{-somaPrimoCollatz::Int->Int
somaPrimoCollatz n1 
  |n1 == 1 = 0
  |primo(collatz n1 ) = n1 + somaPrimoCollatz (collatz n1)
  |otherwise = somaPrimoCollatz (collatz n1)-}

somaprimocollatz::Int->Int
somaprimocollatz n 
  |n == 1 = 0
  |n == 2 = n
  |not(par n) && primo n = n + somaprimocollatz ((3*n) + 1)
  |not(par n) && not(primo n) = somaprimocollatz ((3*n) + 1)
  |otherwise = somaprimocollatz (div n 2)

somaOpcollatz::Int->Int
somaOpcollatz n 
  |n==1 = 0
  |par n = 1 + somaOpcollatz (div n 2)
  |otherwise = 1 + somaOpcollatz ((3*n) + 1)

collatz::Int->Int
collatz n 
  |n==1 = 0
  |par n = collatz (div n 2)
  |otherwise = collatz ((3*n) + 1)
  
par::Int->Bool
par n
  |mod n 2 == 0 = True
  |otherwise = False

aux::Int->Int->Bool
aux n i
   |i==1 = True
   |mod n i==0 = False
   |otherwise = aux n (i-1)

primo::Int->Bool
primo x
   |x==1 = False
   |x==2 = True
   |otherwise = aux x (x-1)