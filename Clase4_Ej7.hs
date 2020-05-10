--============================EJERCICIO 07================================
--Implementar una funcion que dado un n, sume todos los numeros naturales menores o iguales que n que tengan todos los digitos iguales.
once :: Integer -> Integer
once n |cantDigitos (n) == 1 = 1
 |otherwise = 10^( cantDigitos (n) -1 ) + once ( div n 10 )

--Crea reemplaza todos los numeros de un numero dado por 1. Es decir once (333) = 111. Teorema de las Bases

sumaDigitosIguales :: Integer -> Integer
sumaDigitosIguales n |n == 11 = 11 + sumaDigitosIguales (9)
 |digitosiguales n && ( (div n (10^(cantDigitos (n) - 1))) == 1) && n>0 = n + sumaDigitosIguales ( n- once (div n 10) -1 )
 |digitosiguales n && n>0 = n + sumaDigitosIguales ( n- once (n) )
 |otherwise = 0

 -- Primero evalua si es 11, = 11 + Sumatoria(9) (INNECESARIO)
 -- Si es un numero que tiene todos los digitos son iguales, al sacar el ultimo es 1 (por ende todos son 1). Le resta 11**2 para que sea 99*** y sigue la sumatoria de eso
 -- Suma el numero y avanza al proximo que tiene digitos iguales, para esto se le resta -11**
 -- Si no es igual da 0 (Es una manera de expresar el error)