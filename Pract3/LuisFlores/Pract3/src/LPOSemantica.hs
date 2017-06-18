--Logica Computacional 2016-2
--Tema : Implementacion de la semantica de la logica
--de predicados (logica de primer orden)
--Profesor: Favio E. Miranda Perea
--Ayudante: Susana H. Martin Lunas
--Laboratorio: Fernando A. Galicia Mendoza
module LPOSemantica where

	import LPO
	import Semanal2
	
	--Representamos al universo como m::[a]
	
	--Representamos a las relaciones como r::[a] -> Bool
	
	--Representamos a las funciones como f::[a] -> a
	
	--Tipo de la interpretacion de funciones
	type IntF a = Nombre -> [a] -> a
	
	--Tipo de la interpretacion de relaciones
	type IntR a = Nombre -> [a] -> Bool
	
	--Tipo de estados de variables del universo
	type Estado a = Nombre -> a
	
	--Funcion que devuelve la actualización de estados
	actEstados :: Estado a -> Nombre -> a -> Estado a
	actEstados f x m y = if x == y then m else f y
	
	--Funcion que devuelve la interpretacion de un termino
	--respecto a un estado
	iTerm :: Estado a -> IntF a -> Term -> a
	iTerm f h (V x) = f x
	iTerm f h (F c []) = h c []
	iTerm f h (F f1 lt) = h f1 (iTermL f h lt) where
		iTermL f h [] = []
		iTermL f h (t:ts) = (iTerm f h t):(iTermL f h ts)

	--Tipo que representa un mundo
	type Mundo a = (Estado a,IntF a,IntR a)

	--Funcion que devuelve la interpretacion de una fórmulas
	--respecto a un estado
	iForm :: Eq a => [a] -> Mundo a -> Form -> Bool
	iForm m _ TrueF = True
	iForm m _ FalseF = False 
	iForm m (sigma,intf,intr) (Pr p lt) = intr p [iTerm sigma intf t | t <- lt]
	iForm m (sigma,intf,intr) (Eq t1 t2) = (iTerm sigma intf t1) == (iTerm sigma intf t2)
	iForm m w (Neg f) = not (iForm m w f)
	iForm m w (Conj f1 f2) = (iForm m w f1) && (iForm m w f2)
	iForm m w (Disy f1 f2) = (iForm m w f1) || (iForm m w f2)
	iForm m w (Imp f1 f2) = (not (iForm m w f1)) || (iForm m w f2)
	iForm m w (Equi f1 f2) = (iForm m w f1) == (iForm m w f2)
	iForm m (sigma,intf,intr) (All x f) = and [ iForm m ((actEstados sigma x e),intf,intr) f | e <- m] 
	iForm m (sigma,intf,intr) (Ex x f) = or [ iForm m ((actEstados sigma x e),intf,intr) f | e <- m]

	--Ejemplo

	--Universo : Z10, es decir, los enteros módulo 10

	m :: [Int]
	m = [0..9]

	est :: Estado Int
	est "x" = 1
	est "y" = 2
	est _ = 0

	iF :: IntF Int
	iF s [] = case s of
		"0" -> 0
		"1" -> 1
		"2" -> 2
		"3" -> 3
		"4" -> 4
		"5" -> 5
		"6" -> 6
		"7" -> 7
		"8" -> 8
		"9" -> 9
		_	-> 0
	iF "id" [n] = n
	iF "+" [n1,n2] = (mod n1 10)+(mod n2 10)
	iF "-" [n1,n2] = (mod n1 10)-(mod n2 10)
	iF "*" [n1,n2] = (mod n1 10)*(mod n2 10)
	iF "/" [n1,n2] = div (mod n1 10) (mod n2 10)

	iM :: IntR Int
	iM "<=" [n1,n2] = n1 <= n2
	iM "=" [n1,n2] = n1 == n2

	--id 1 = 1
	ejemplo1 = iTerm est iF (F "id" [F "1" []])

	--1+2 = 3
	ejemplo2 = iTerm est iF (F "+" [F "1" [],F "2" []])

	--3 / 0 = 0
	ejemplo3 = iTerm est iF (F "/" [F "0" [],F "3" []])

	--Para todo x en Z10, 0 <= x
	ejemplo4 = iForm m (est,iF,iM) (All "x" (Pr "<=" [F "0" [],V "x"]))

	--Para todo x en Z10, existe un y en Z10 tal que x <= y
	ejemplo5 = iForm m (est,iF,iM) (All "x" (Ex "y" (Pr "<=" [V "x", V "y"])))

	--Falso: Para todo x en Z10, 0 = x 
	ejemplo6 = iForm m (est,iF,iM) (All "x" (Pr "=" [F "0" [], V "x"]))

	--Para todo x en Z10, 0 = x - x
	ejemplo7 = iForm m (est,iF,iM) (All "x" (Pr "=" [F "0" [], F "-" [V "x", V "x"]]))

	--Para todo x en Z10, x = id(x)
	ejemplo8 = iForm m (est,iF,iM) (All "x" (Pr "=" [V "x", F "id" [V "x"]]))

	--Para todo x en Z10, existe un y en Z10, tal que x = y + y
	ejemplo9 = iForm m (est,iF,iM) (All "x" (Ex "y" (Pr "=" [V "x", F "+" [V "y",V "y"]])))