import FuncionesBasicas

esSumaInicialDePrimos :: Int -> Int -> Bool --Input del usuario
esSumaInicialDePrimos n m = eSIDP n m 1 1

eSIDP :: Int -> Int -> Int -> Int -> Bool --n el parametro numero_deseado, m el parametro numero_maximo, g es variable_primo1, j es la variable_primo2
eSIDP n m g j |g>m || j>m || (n > (2*m)) = False --Limitadores de Funcion // Porque si es la suma de dos numeros, no puede ser mayor a dos veces el maximo 
 |(n<m || n==m) && (esPrimo(n) || esPrimo (n-2)) = True --Si n es menor igual a m Caso (n+0=n) o (x+2=n)
 |esPrimo(g) && esPrimo(j) && ( g+j )==n = True --Lo que buscamos
 |g==j = eSIDP n m (g+1) j --Asi primero aumenta g
 |otherwise = eSIDP n m g (j+1) --Asi aumenta j
