
--1
par :: Int -> Bool
par n = (mod n 2 == 0)

impar :: Int -> Bool
impar n = not(par n )
--a
impares :: [Int]
impares =[n | n <- [1 .. 100], impar n] --mod n 2 /= 0
--b
pares :: [Int]
pares = [a | a <- [10 .. 100], par a]
--c
imparesN :: Int -> [Int]
imparesN x =[n | n <- [1 .. x], mod n 2 /= 0]
--d
mut3e5 :: Int -> [Int]
mut3e5 x =[n | n <- [1 .. x], mod n 3 == 0 && mod n 5 == 0]--ou usar virgula ao inves do &&
--e
quad :: Int -> [(Int, Int)]
quad x = [(z,z*z) | z <- [1 .. x]]
--f
matriz :: [(Int, Int)]
matriz = [ (i,j) | i <- [1..3], j<- [1..4]]
--g
matrizNM :: Int -> Int -> [(Int, Int)]
matrizNM c v = [ (i,j) | i <- [1..c], j<- [1..v]]

--2
listaFib::Int->[Int]
listaFib n = listaFibAux n 0

listaFibAux::Int->Int->[Int]
listaFibAux limite contador 
  |contador == limite = []
  |otherwise = [fib (contador)] ++ listaFibAux limite (contador+1)

fib::Int->Int
fib n 
  |n==0 = 0
  |n==1 = 1
  |otherwise = fib (n-1) + fib (n-2)

--3
hexadecimal::String->String
hexadecimal string
   |length string == 0 = []
   |mod (length string) (4) /= 0 = hexadecimal(['0' |x <- [1..(4 - mod (length string) (4))]] ++ string)
   |otherwise = binToHex (take 4 string) ++ binToHex (drop 4 string)

binToHex::String->String
binToHex "0000" = "0"
binToHex "0001" = "1"
binToHex "0010" = "2"
binToHex "0011" = "3"
binToHex "0100" = "4"
binToHex "0101" = "5"
binToHex "0110" = "6"
binToHex "0111" = "7"
binToHex "1000" = "8"
binToHex "1001" = "9"
binToHex "1010" = "A"
binToHex "1011" = "B"
binToHex "1100" = "C"
binToHex "1101" = "D"
binToHex "1110" = "E"
binToHex "1111" = "F"
binToHex _= ""

--4
hanoi :: Int -> Int -> Int -> Int -> [String]
hanoi q orig aux dest
   | q == 0 = []
   | q == 1 = [show(orig) ++"->"++show(dest)]
   | otherwise = ch1 ++ [show(orig)++ "->"++show(dest)]++ch2
      where
         ch1 = hanoi (q-1) orig dest aux
         ch2 = hanoi (q-1) aux orig dest





