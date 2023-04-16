
contFat :: Int->Int->Int->Int
contFat n y z
   |y==0 = 1
   |y>n = 0
   |otherwise = 1 + contFat n (y*z) (z+1)
