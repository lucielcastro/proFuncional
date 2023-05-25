{-CRiar função chamada pot2(b, e) que apresente o resultado da potência da base "b" elevada ao expoente "e" respeitando as condições matematicas que regimentam a operação-}

pot :: Int -> Int -> Int
pot b e = potBs b e 1

potBs :: Int -> Int -> Int -> Int
potBs b e v
   | e == 0 = v
   | e == 1 = v * b
   | otherwise = potBs b (e-1) b * v


{-
pot(x, y) = {

|1, se y = 0
|x * pot(x, y − 1) se y > 0

div(x, y) =

|0, se x < y
|1, se x = y
|1 + div(x − y, y), se x > y

mod(x, y) =

|0, se x = y
|x, se x < y
|mod(x − y, y), se x > y


primo(x) =

|F, se x = 1
|V, se x = 2
|aux(x, x − 1), c.c.


aux(n, i) =

V, se i = 1
F, se mod(n, i) = 0
aux(n, i − 1), c.c. Continua a busca

---------------------------------------------------
primo : N+ → B

primo(x) =

F, se x = 1
V, se x = 2
V, contadiv(x, x) = 2
F, c.c.
contadiv : N+ × N+ → N+

contadiv(n, i) =

1, se i = 1
1 + contadiv(n, i − 1), se mod(n, i) = 0
contadiv(n, i − 1), c.c.

-------------------------------------------------
perfeito : N+ → B

perfeito(x) =

F, se x = 1
V, se somaDiv(x, x − 1) = x
F, c.c.

somaDiv : N+ × N+ → N+

somaDiv(x, i) =

1, se i = 1
i + somaDiv(x, i − 1), se mod(x, i) = 0
somaDiv(x, i − 1), c.c.

-}