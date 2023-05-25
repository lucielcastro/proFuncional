venda :: Int -> Int
venda 0 = 5
venda 1 = 6
venda 2 = 4
venda 3 = 8
venda 4 = 2
venda 5 = 7
venda _ = 0

nAnd::Bool->Bool->Bool
nAnd x y
  |(x==True) && (y==True)=False
  |otherwise=True
-- nAnd True True = False
-- nAnd _ _ = True

numEquallMax::Int->Int->Int->Int
numEquallMax m n p
  |(n==m)&&(m==p)=3
  |(n/=m)&&(m/=p)&&(n/=p)=1
  |otherwise=2

qualSemanaSemVenda::Int->Int
qualSemanaSemVenda n
  |n<0 = -1
  |venda n==0 = n
  |otherwise = qualSemanaSemVenda (n-1)

semanaSemVenda::Int->Bool
semanaSemVenda n
  |n<0 = False
  |venda n==0 = True
  |otherwise = semanaSemVenda (n-1)

maiorValorVenda :: Int -> Int
maiorValorVenda n
  | n == 0 = venda 0
  -- | venda n > maiorValorVenda (n -1) = venda n
  | otherwise = max (venda n) (maiorValorVenda (n -1))

maiorSemanaVenda :: Int -> Int
maiorSemanaVenda n
  | n == 0 = 0
  | venda n > venda (maiorSemanaVenda (n-1)) = n
  |otherwise = maiorSemanaVenda (n-1)

{--
primo 4 => primoAux 4 3 => primoAux 4 2 => False
primo 5 => primoAux 5 4 => primoAux 5 3 => => primoAux 5 2 => => primoAux 5 1 => True
--}

somaDig :: Int -> Int
somaDig n
  | n < 10 = n
  | otherwise = (n `mod` 10) + somaDig (div n 10)

padovan :: Int -> Int
padovan n
  | n <= 2 = 1
  | otherwise = padovan (n -2) + padovan (n -3)


parGemeo::Int->Int->Bool
parGemeo x y
  |(primo x) && (primo y) && (y-x==2) = True
  |otherwise=False

pertencePar::Int->Bool
pertencePar n
  |parGemeo n (n+2)|| parGemeo (n-2) n = True
  -- |primo n && (primo (n+2) || primo (n-2))=True
  |otherwise=False

contaParGemeo::Int->Int
contaParGemeo n
  |n<5=0
  |parGemeo (n-2) n = 1+ contaParGemeo (n-1)
  |otherwise= contaParGemeo (n-1)

somaParGemeo::Int->Int
somaParGemeo n
  |n<5=0
  |parGemeo (n-2) n = n+(n-2) + somaParGemeo (n-1)
  |otherwise= somaParGemeo (n-1)

primo :: Int -> Bool
primo n = primoAux n (n -1)

primoAux :: Int -> Int -> Bool
primoAux n d
  | n == 1 = False
  | n == 2 = True
  | d == 1 = True
  | mod n d == 0 = False
  | otherwise = primoAux n (d -1)


par::Int->Bool
-- par n = ((mod n 2)==0)
par n
  |mod n 2 == 0 = True
  |otherwise = False

impar::Int->Bool
impar n = not (par n)
-- impar n = ((mod n 2)/=0)

somaAlgPar::Int->Int
somaAlgPar n
  |(n<10) && (par n) = n
  |(n<10) && (impar n) = 0
  |par(resto) = (resto) + somaAlgPar (quo)
  |otherwise = somaAlgPar (quo)
    where 
      resto = mod n 10
      quo= div n 10

contAlgImpar::Int->Int
contAlgImpar n
  |(n<10) && (par n) = 0
  |(n<10) && (impar n) = 1
  |impar(mod n 10) = 1 + contAlgImpar (div n 10)
  |otherwise = contAlgImpar (div n 10)

multAlgPrimos::Int->Int
multAlgPrimos n
  |(n<10) && (primo n) = n
  |(n<10) && not (primo n) = 1
  |primo(mod n 10) = (mod n 10) * multAlgPrimos (div n 10)
  |otherwise = multAlgPrimos (div n 10)


fib::Int->Int->Int->Int
fib n p s
  |(n==1) && primo s = s
  |primo s = fib (n-1) s (p+s)
  |otherwise= fib n s (p+s)

nPrimoFibo::Int->Int
nPrimoFibo n = fib n 1 1

{--
primoSG 4 => primo 4 && primo 9 => False && primo 9 => False
primoSG 7 => primo 7 && primo 15 => True && primo 15 => True && False => False
primoSG 11 => primo 11 && primo 23 => True && primo 23 => True && True => True
--}

primoSG :: Int -> Bool
primoSG n = (primo n) && (primo (2 * n + 1))

{--
contPrimoSG 4 => primoSG 4 => contPrimoSG 3 => primoSG 3 => 1+contPrimoSG 2 => 1+primoSG 2 => 1+1+contPrimoSG 1 => 1+1+0 => 2
contPrimoSG 7 => primoSG 7 => contPrimoSG 6 => contPrimoSG 5 => 1+contPrimoSG 4 => 1+contPrimoSG 3 => 1+1+contPrimoSG 2 =>
    1+1+1+contPrimoSG 1=> 1+1+1+0 => 3
--}

contPrimoSG :: Int -> Int
contPrimoSG n
  | n == 1 = 0
  | primoSG n = 1 + contPrimoSG (n -1)
  | otherwise = contPrimoSG (n -1)

concatena :: [[t]] -> [t]
concatena [] = []
concatena (a : x) = a ++ (concatena x)