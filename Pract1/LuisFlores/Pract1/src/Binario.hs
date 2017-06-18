{-
- Equipo:
- Garcia Argueta Jaime Daniel, 312104739, jaimegarciaargueta@ciencias.unam.mx
- Flores González Luis Brandon, 312218342, iluis@ciencias.unam.mx
-}
module Binario where

	{-
	-Tipo de datos binario: Iniciando desde el uno
	-Base Uno --> 1
	-Cero x -> x0
	-Uno x -> x1
	-Es decir, Cero agrega el digito 0 como menos significativo,
	-lo mismo que Uno con 1.
	-}
	data Binario = BaseUno | Cero Binario | Uno Binario

	{-
	-Instancia de la clase show, para mostrarlos como cadenas de caracteres de
	-1's y 0's.
	-}
	instance Show Binario where
		show BaseUno = show(1)
		show (Uno x) = show (x) ++ show (1)
		show (Cero x) = show (x) ++ show (0)

	--Funcion sucesor: Recibe un binario y devuelve su sucesor
	sucesor :: Binario -> Binario
	sucesor BaseUno = Cero BaseUno
	sucesor (Cero x) = Uno x
	sucesor (Uno x) = Cero (sucesor x)

	--Funcion suma: Recibe dos binarios y realiza su suma
	suma :: Binario -> Binario -> Binario
	suma BaseUno x = sucesor x
	suma x BaseUno = sucesor x
	suma (Cero x) (Uno y) = Uno (suma x y)
	suma (Uno x) (Cero y) = Uno (suma x y)
	suma (Cero x) (Cero y) = Cero (suma x y)
	suma (Uno x) (Uno y) = Cero (sucesor(suma x y))

	--Funcion predecesor: Recibe un binario y devuelve su binario anterior
	predecesor :: Binario -> Binario
	predecesor BaseUno = BaseUno
	predecesor (Cero BaseUno) = BaseUno 
	predecesor (Uno x) = Cero x
	predecesor (Cero x) = Uno (predecesor x)

	--Funcion natBinLista: Recibe un natural (mayor que 1) y devuelve una lista de enteros representando
	--su notacion binaria
	natBinLista :: Int -> [Int]
	natBinLista 0 = []
	natBinLista x = makeBinList (natABin x)

	--Funcion sumBinLista: Recibe dos binarios (representados en lista) y devuelve su suma en el tipo
	--de datos Binario.
	sumaBinLista :: [Int] -> [Int] -> Binario
	sumaBinLista x y = natABin(binANat((binListToBin x)) + binANat(binListToBin(y)))
	
	--Puntos extra

	--Funcion natABin: Recibe un natural (mayor que 1) y devuelve un binario
	natABin :: Int -> Binario
	natABin 1 = BaseUno
	natABin x = if(x < 1)
		then (error "Solo numeros mayores a 0.") 
		else (if (even x) then (Cero (natABin (div x 2))) else (Uno (natABin (div x 2))))

	--Funcion binANat: Recibe un binario y devuelve su reprentacion en numero natural (mayor que 1).
	binANat :: Binario -> Int
	binANat BaseUno = 1
	binANat (Cero x) = (binANatPow x 1)
	binANat (Uno x) = 1 + (binANatPow x 1)

-----------------------------Auxiliares.------------------------

	--Funcion makeBinList: Funciòn auxiliar que recibe un binario y devuelve una lista de 0's y 1's de sus
	--bits respectivos.
	makeBinList :: Binario -> [Int]	
	makeBinList BaseUno = [1]	
	makeBinList (Cero x) = (makeBinList x) ++ [0]
	makeBinList (Uno x) = (makeBinList x) ++ [1]

	--Funcion binListaABin: Funciòn que recibe un binario en su representacion de lista y devuelve el mismo
	--numero pero como tipo Binario e invertido.
	binListaABin :: [Int] -> Binario
	binListaABin [] = error "Lista vacìa."
	binListaABin [1] = BaseUno
	binListaABin (x:xs) = if (x == 1) then (Uno (binListaABin xs)) else (Cero (binListaABin(xs)))

	--Funcion binListToBin: Funciòn que recibe un binario en su representacion de lista y llama a la funcion
	--binListABin con la lista invertida para convertir el numero de su representacion de lista a uno de tipo Binario.
	binListToBin :: [Int] -> Binario
	binListToBin x	= binListaABin (invertList x)

	--Funcion invertList: Funciòn auxiliar que recibe una lista de enteros y devuelve la misma lista pero invertida.
	invertList :: [Int] -> [Int]
	invertList [] = []
	invertList (x:xs) = (invertList xs) ++ [x]

	--Funcion binANatPow: Recibe un binario, un exponente y devuelve el numero 2 elevado al exponente si el bit menos 
	--significativo del binario es uno y devuelve cero si el bit menos significativo es cero.
	binANatPow :: Binario -> Int -> Int
	binANatPow BaseUno  n = ((^)2 n)
	binANatPow (Cero x) n = binANatPow x (n+1)
	binANatPow (Uno x) n = ((^)2 n) + binANatPow x (n+1)	
