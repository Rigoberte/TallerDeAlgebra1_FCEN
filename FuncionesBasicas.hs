module FuncionesBasicas
where

suma :: Int -> Int -> Int
suma x y = x + y

esPar :: Int -> Bool
esPar x = mod x 2 == 0

esImpar :: Int -> Bool
esImpar x = not(esPar x)

normaVectorial :: (Float, Float) -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)

signo :: Float -> Int
signo x | x == 0 = 0
 |(x > 0) = 1
 |otherwise = (-1)

maximo :: Float -> Float -> Float
maximo x y |x > y = x
 |otherwise = (y)

minimo :: Float -> Float -> Float
minimo x y |x > y = y
 |otherwise = (x)

restaN :: Int -> Int -> Int
restaN n m|((m > n) || (m == n)) = 0
 |otherwise = n - m

sumatoria :: Int -> Int
sumatoria n|n==1 = 1
 |otherwise = n + sumatoria(n-1)

factorial :: Int -> Int
factorial n|n == 0 = 1
 |otherwise = n * factorial (n-1)