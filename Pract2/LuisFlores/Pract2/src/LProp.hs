{-
-Modulo que representa la sintaxis de la logica proposicional
-y las funciones numvars, vars
-Logica computacional 2016-2
-Profesor: Favio E. Miranda Perea
-Ayudante: Susana H. Martin Lunas
-Laboratorio: Fernando A. Galicia Mendoza
-}

module LProp where

	import Data.List

	--Representamos las variables proposicionales como enteros
	type VarP = Int

	--Tipo de datos que representan las formulas de la lógica proposicional
	data Prop = Top | Bot | P VarP | Neg Prop | Or Prop Prop | And Prop Prop | Impl Prop Prop | Syss Prop Prop

	--Instancia para mostrar las formulas
	instance Show Prop where
		show Top = "T"
		show Bot = "F"
		show (P x) = "P "++show x
		show (Neg p) = "~ "++show p
		show (Or p1 p2) = "("++(show p1)++" v "++(show p2)++")"
		show (And p1 p2) = "("++(show p1)++" ^ "++(show p2)++")"
		show (Impl p1 p2) = "("++(show p1)++" -> "++(show p2)++")"
		show (Syss p1 p2) = "("++(show p1)++" <-> "++(show p2)++")"

	--Instancia para la igualdad de formulas
	instance Eq Prop where
		Top == Top = True
		Bot == Bot = True
		(P x) == (P y) = x == y
		(Or p1 p2) == (Or q1 q2) = (p1 == q1) && (p2 == q2)
		(And p1 p2) == (And q1 q2) = (p1 == q1) && (p2 == q2)
		(Impl p1 p2) == (Impl q1 q2) = (p1 == q1) && (p2 == q2)
		(Syss p1 p2) == (Syss q1 q2) = (p1 == q1) && (p2 == q2)
		_ == _ = False

	--Funcion que devuelve el numero de variables proposicionales
	numVars :: Prop -> Int
	numVars Top = 0
	numVars Bot = 0
	numVars (P x) = 1
	numVars (Neg p) = numVars p
	numVars (Or p1 p2) = (numVars p1)+(numVars p2)
	numVars (And p1 p2) = (numVars p1)+(numVars p2)
	numVars (Impl p1 p2) = (numVars p1)+(numVars p2)
	numVars (Syss p1 p2) = (numVars p1)+(numVars p2)

	--Funcion que devuelve el conjunto de variables proposicionales
	vars' :: Prop -> [VarP]
	vars' Top = []
	vars' Bot = []
	vars' (P x) = [x]
	vars' (Neg p) = (vars' p)
	vars' (Or p1 p2) = (vars' p1)++(vars' p2)
	vars' (And p1 p2) = (vars' p1)++(vars' p2)
	vars' (Impl p1 p2) = (vars' p1)++(vars' p2)
	vars' (Syss p1 p2) = (vars' p1)++(vars' p2)

	--Funcion quitaRep: Elimina los elementos repetidos en una lista
	--A la última lista se le llama acumulador
	--Un acumulador es una estrucutura donde como su nombre lo indica, acumula las cosas
	--que le vayas dando, en este caso acumula en una lista
	quitaRep :: Eq a => [a] -> [a] -> [a]
	quitaRep [] l = l
	--Con condicional: quitaRep (x:xs) l = if elem x l then (quitaRep xs l) else (quitaRep xs (x:l))
	--Sin condicional: Utilizando case
	quitaRep (x:xs) l = case (elem x l) of
		True -> (quitaRep xs l)
		False -> (quitaRep xs (x:l))

	--Función que devuelve el conjunto de variables proposicionales (sin repeticion)
	vars :: Prop -> [VarP]
	vars phi = quitaRep (vars' phi) [] 
