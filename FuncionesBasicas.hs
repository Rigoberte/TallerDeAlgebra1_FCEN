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

restaNatural :: Int -> Int -> Int
restaNatural n m|((m > n) || (m == n)) = 0
 |otherwise = n - m

sumatoria :: Int -> Int
sumatoria n|n==1 = 1
 |otherwise = n + sumatoria(n-1)

factorial :: Int -> Int
factorial n|n == 0 = 1
 |otherwise = n * factorial (n-1)

digitosiguales:: Integer -> Bool
digitosiguales n |n < 10 = True
 |otherwise = ( (mod n 10) == (mod (div n 10) 10) ) && digitosiguales (div n 10)

cantDigitos:: Integer -> Integer
cantDigitos n |(n < 10) = 1
 |otherwise = 1 + cantDigitos (div n 10)

esDivisor:: Integer -> Integer -> Bool
esDivisor n m = mod n m == 0

esPotencia:: Integer -> Integer -> Bool
esPotencia n m =  n == 1 || n >= m &&  esPotencia (div n m) m && mod n m == 0

tieneDivisoresDesde :: Int -> Int -> Bool
tieneDivisoresDesde n k |k == n = False
 |otherwise = (mod n k == 0) || tieneDivisoresDesde n ( k + 1 )

esPrimo:: Int -> Bool
esPrimo n = (n > 1) && not (tieneDivisoresDesde n 2)
