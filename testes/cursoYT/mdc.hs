mdc :: Int -> Int -> Int
mdc a b
   | b == 0 = a
   | otherwise = mdc b (mod a b)