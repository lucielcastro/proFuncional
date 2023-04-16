qtdAlg :: Integer -> Integer
qtdAlg n 
   | n > -10 && n < 10 = 1
   | otherwise = 1 + qtdAlg(div n 10)