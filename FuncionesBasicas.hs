module FuncionesBasicas
where

suma:: Int -> Int -> Int
suma x y = x + y

esPar:: Int -> Bool
esPar x = mod x 2 == 0

esImpar:: Int -> Bool
esImpar x = not(esPar x)

normaVectorial:: (Float, Float) -> Float
normaVectorial (x, y) = sqrt(x^2 + y^2)

signo:: Float -> Int
signo x | x == 0 = 0
 |(x > 0) = 1
 |otherwise = (-1)

maximo:: Float -> Float -> Float
maximo x y |x > y = x
 |otherwise = (y)

minimo:: Float -> Float -> Float
minimo x y |x > y = y
 |otherwise = (x)

restaNatural:: Int -> Int -> Int
restaNatural n m|((m > n) || (m == n)) = 0
 |otherwise = n - m

sumatoria:: Int -> Int
sumatoria n|n==1 = 1
 |otherwise = n + sumatoria(n-1)

factorial:: Integer -> Integer
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

--Listas======================================
--L===========================================

len:: Set Int -> Int
len list |tail list == [] = 1
 |otherwise = 1 + len (tail list)

pertenece::[Int] -> Int -> Bool
pertenece list m|list==[] = False
 |head list == m = True
 |otherwise = pertenece (tail list) m

union::Set Int -> Set Int -> Set Int
union [] c2 = c2
union c1 c2 |not(pertenece c2 (head c1)) = union (tail c1) (head c1:c2)
 |otherwise = union (tail c1) c2

interseccion::Set Int -> Set Int -> Set Int
interseccion c1 c2 = intersec c1 c2 []

intersec::Set Int -> Set Int -> Set Int -> Set Int
intersec [] c2 c3 = c3
intersec c1 c2 c3 |pertenece c2 (head c1) = intersec (tail c1) c2 (head c1:c3)
 |otherwise = intersec (tail c1) c2 c3

voltereta:: Set Int -> Set Int
voltereta list = vuelta list []

vuelta::Set Int -> Set Int -> Set Int
vuelta list res |tail list == [] = head list:res
 |otherwise = vuelta (tail list) (head list:res)
