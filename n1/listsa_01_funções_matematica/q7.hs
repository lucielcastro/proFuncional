{-Funcionando-}
pg::Int->Int->Int->Int
pg q a n
  |n==1 = 1
  |otherwise = q * pg q a (n-1)