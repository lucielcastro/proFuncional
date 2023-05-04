
mmc::Int->Int->Int
mmc x y = div (x*y) (mdc x y) 

mdc::Int->Int->Int
mdc x y
  |x==y = x
  |x>y = mdc (x-y) y
  |otherwise = mdc y x