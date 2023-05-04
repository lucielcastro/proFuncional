somaPrimoCollatz::Int->Int->Int
somaPrimoCollatz n1 n2
  |n1==n2 = 1
  |primo n1  = n1 + somaPrimoCollatz (collatz n1) n2
  |otherwise = somaPrimoCollatz (collatz n1) n2

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