primoPadovan :: Int -> Int
primoPadovan n = pPadovanAux n 1

pPadovanAux :: Int -> Int -> Int
pPadovanAux n x
   | primo(padovan x) && n == 1 = padovan x
   | primo(padovan x) && n > 1 = pPadovanAux (n-1) (x+1)
   | otherwise = pPadovanAux n (x+1)

padovan :: Int -> Int
padovan n
   | n == 0 = 1
   | n == 1 = 1
   | n == 2 = 1
   | otherwise = padovan (n-2) + padovan (n-3)


primo :: Int -> Bool
primo n
   | n < 2     = False
   | otherwise = aux n (n-1)
        
aux :: Int -> Int -> Bool
aux n i
   | i == 1        = True
   | mod n i == 0  = False
   | otherwise     = aux n(i-1)