{-Funcionando-}
padovan::Int->Int
padovan n
  |n==0 = 1
  |n==1 = 1
  |n==2 = 1
  |otherwise = padovan (n-2) + padovan(n-3)