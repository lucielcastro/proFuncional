
nMax::Int->Int->Int->Int
nMax n m p
  |n==m && m==p = 3
  |n==m && m/=p = 2
  |n==p && p/=m = 2
  |p==m && m/=n = 2
  |otherwise = 1

nAnd::Bool->Bool->Bool
nAnd n1 n2
   |n1==True && n2==True = False
   |otherwise = True

multPrimo::Int->Int
multPrimo n 
     |n<10 && primo n = n 
     |n<10 && not (primo n) = 1
     |primo (mod n 10) = mod n 10 * multPrimo (div n 10)
     |otherwise =  multPrimo (div n 10)
   
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

fibonaci::Int->Int->Int->Int
fibonaci n p s
       |n==1 && primo s = s
       |primo s = fibonaci (n-1) s (p+s)
       |otherwise = fibonaci n s (p+s)
     
nPrimoFib::Int->Int
nPrimoFib n = fibonaci n 1 1
     
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
       
       somaPar::Int->Int
       somaPar n
         |n < 10 && par n = n
         |n < 10 && not (par n) = 0
         |par (mod n 10) = mod n 10 + somaPar (div n 10)
         |otherwise = somaPar (div n 10)
       
       par::Int->Bool
       par n
         |mod n 2 == 0 = True
         |otherwise = False
       
       impar::Int->Bool
       impar n
         |mod n 2 /= 0 = True
         |otherwise = False