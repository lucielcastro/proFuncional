type Hora = (Int, Int, Int)

valida :: Hora ->Bool
valida (hora, minutos, seg)
   | hora < 0 || hora > 23 = False
   | minutos < 0 || minutos>59 = False
   | seg < 0 || seg > 59 = False
   | otherwise = True

totalSegundo :: Hora -> Int
totalSegundo ( hor, min, seg) = seg + min*60 + hor*60*60


converteSegundo :: Int -> Hora
converteSegundo total = (hora, min, seg)
   where
      hora = div total 3600
      min = div (mod total 3600 ) 60
      seg = mod(mod total 3600) 60

diHora :: Hora -> Hora -> Hora
diHora hora1 hora2 = converteSegundo(totalSegundo(hora1) - totalSegundo(hora2))