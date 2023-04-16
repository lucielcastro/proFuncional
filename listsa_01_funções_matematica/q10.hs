coPrimo :: Int -> Int -> Int -> Int
coPrimo n d i
  | i == 1 = True
  | mod n i == 0 && mod d i == 0 = False
  | otherwise coPrimo n d i-1

contTotiente :: Int->Int->Int
contTotiente n a
  | a == 1 = 1
  | 1 + contTotiente n a-1 coPrimo n a a = True
  | otherwise contTotiente n a-1
