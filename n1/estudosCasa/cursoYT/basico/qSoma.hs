{-Criar uma função qsoma(a b) que recebe dois valores numéricos e retorne o resultado do quadradado da soma dos valores fornecidos-}
quadrado :: Int -> Int
quadrado n = n * n

qsoma :: Int -> Int -> Int
qsoma n x = quadrado(n+x)