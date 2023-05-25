somaDig :: Int -> Int
somaDig x
   | x < 10 = x
   |otherwise  = mod x 10 + somaDig(div x 10 )