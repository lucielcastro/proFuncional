pertenceGemeo::Int->Bool
pertenceGemeo n
  |primoGemeo n (n+2) || primoGemeo n (n-2) = True
  |otherwise = False

primoGemeo::Int->Int->Bool
primoGemeo n1 n2
  |(primo n1 && primo n2) && (n1 + 2) == n2 || (primo n2 && primo n1) && (n1 - 2) == n2 = True
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