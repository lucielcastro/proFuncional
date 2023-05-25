--EU FIZ
primoMercene::Int->Bool
primoMercene n = primo (2^n - 1)

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

--MONITOR FEZ

mercenne::Int->Bool
mercenne n
  |not (primo n) = False
  |otherwise = auxMercene n 1

nPrimo::Int->Int
nPrimo n = contPrimo 0 2
   where
      contPrimo::Int->Int->Int
      contPrimo cont num
        |cont==n = num-1
        |primo num = contPrimo (cont+1) (num+1)
        |otherwise = contPrimo cont (num+1)

auxMercene::Int->Int->Bool
auxMercene num cont
  |2^cont-1 == num = True
  |2^cont-1 > num = False
  |otherwise = auxMercene num (cont+1)

-- MONITOR FEZ 
-- CALCULA O ENESIMO PRIMO DE MERCENNE
nPrimoMercene::Int->Int
nPrimoMercene n = contMercene 0 2
  where
    contMercene::Int->Int->Int
    contMercene cont num
      |cont==n = num-1
      |primoMercene num = contMercene (cont+1) (num+1)
      |otherwise = contMercene cont (num+1)

auxNMercene::Int->Int->Int
auxNMercene num cont
  |num==1 && mercenne (nPrimo cont) = nPrimo cont
  |mercenne (nPrimo cont) = auxNMercene (num-1) (cont+1)
  |otherwise = auxNMercene num (cont+1)