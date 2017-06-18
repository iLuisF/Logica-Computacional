--Logica Computacional 2016-2
--Tema : Implementacion del lenguaje de la logica
--de predicados (logica de primer orden)
--Profesor: Favio E. Miranda Perea
--Ayudante: Susana H. Martin Lunas
--Laboratorio: Fernando A. Galicia Mendoza
module LPO where

	--Modulo de Listas brindados por el lenguaje
	import Data.List

	--Tipo que representara el nombre 
	type Nombre = String

	--Tipo que representa los terminos
	--Requerimos la clase Eq, para la funcion union
	data Term = V Nombre | F Nombre [Term] deriving(Show,Eq)

	--Tipo que representa las formulas
	--Requerimos la clase Eq, para la funcion union
	data Form = TrueF | FalseF | Pr Nombre [Term] | Eq Term Term | Neg Form
				| Conj Form Form | Disy Form Form | Imp Form Form | Equi Form Form
				| All Nombre Form | Ex Nombre Form deriving(Show,Eq)

	--Funcion que devuelve la lista de subterminos de un termino dado
	subt :: Term -> [Term]
	subt (V s) = [V s]
	subt (F s []) = [F s []]
	subt (F s xs) = union [F s xs] (subtL xs) where
	subtL [] = []
	subtL (x:xs) = union (subt x) (subtL xs)

	--Funcion que devuelve el numero de conectivos y cuantificadores sobre una formula
	peso :: Form -> Int
	peso TrueF = 0
	peso FalseF = 0
	peso (Pr s t) = 0
	peso (Eq f1 f2) = 1
	peso (Neg f) = (peso f)+1
	peso (Conj f1 f2) = (peso f1)+(peso f2)+1
	peso (Disy f1 f2) = (peso f1)+(peso f2)+1
	peso (Imp f1 f2) = (peso f1)+(peso f2)+1
	peso (Equi f1 f2) = (peso f1)+(peso f2)+1
	peso (All x p) = 1+(peso p)
	peso (Ex x p) = 1+(peso p)

	--Funcion que devuelve la lista con todos los nombres de constantes que figuran en t
	consT :: Term -> [Nombre]
	consT (V x) = []
	consT (F s []) = [s]
	consT (F s xs) = consTL xs where
		consTL [] = []
		consTL (x:xs) = union (consT x) (consTL xs)

	--Funcion que devuelve la lista con todos los nombres de las variables que figuran en t
	varT :: Term -> [Nombre]
	varT (V x) = [x]
	varT (F s []) = []
	varT (F s xs) = varTL xs where
		varTL [] = []
		varTL (x:xs) = union (varT x) (varTL xs)

	--Funcion que devuelve la lista con todos los nombres de funcion que figuran en t
	funT :: Term -> [Nombre]
	funT (V x) = []
	funT (F c []) = []
	funT (F s xs) = union [s] (funTL xs) where
		funTL [] = []
		funTL (x:xs) = union (funT x) (funTL xs)

	--Funcion que devuelve la lista con todos los nombres de constantes de una formula
	consF :: Form -> [Nombre]
	consF TrueF = []
	consF FalseF = []
	consF (Pr p lt) = concat (map consT lt)
	consF (Eq t1 t2) = union (consT t1) (consT t2)
	consF (Neg f) = consF f
	consF (Conj f1 f2) = union (consF f1) (consF f2)
	consF (Disy f1 f2) = union (consF f1) (consF f2)
	consF (Imp f1 f2) = union (consF f1) (consF f2)
	consF (Equi f1 f2) = union (consF f1) (consF f2)
	consF (All x f) = consF f
	consF (Ex x f) = consF f

	--Funcion que devuelve la lista con todos los nombres de variables de una formula
	varF :: Form -> [Nombre]
	varF TrueF = []
	varF FalseF = []
	varF (Pr p lt) = concat (map varT lt)
	varF (Eq t1 t2) = union (varT t1) (varT t2)
	varF (Neg f) = varF f
	varF (Conj f1 f2) = union (varF f1) (varF f2)
	varF (Disy f1 f2) = union (varF f1) (varF f2)
	varF (Imp f1 f2) = union (varF f1) (varF f2)
	varF (Equi f1 f2) = union (varF f1) (varF f2)
	varF (All x f) = union [x] (varF f)
	varF (Ex x f) = union [x] (varF f)

	--Funcion que devuelve la lista con todos los nombres de función de una formula
	funF :: Form -> [Nombre]
	funF TrueF = []
	funF FalseF = []
	funF (Pr p lt) = concat (map funT lt)
	funF (Eq t1 t2) = union (funT t1) (funT t2)
	funF (Neg f) = funF f
	funF (Conj f1 f2) = union (funF f1) (funF f2)
	funF (Disy f1 f2) = union (funF f1) (funF f2)
	funF (Imp f1 f2) = union (funF f1) (funF f2)
	funF (Equi f1 f2) = union (funF f1) (funF f2)
	funF (All x f) = funF f
	funF (Ex x f) = funF f

	--Funcion que devuelve las variables libres de una formula
	fv :: Form -> [Nombre]
	fv TrueF = []
	fv FalseF = []
	fv (Pr p lt) = concat (map varT lt)
	fv (Eq t1 t2) = union (varT t1) (varT t2)
	fv (Neg f) = fv f
	fv (Conj f1 f2) = union (fv f1) (fv f2)
	fv (Disy f1 f2) = union (fv f1) (fv f2)
	fv (Imp f1 f2) = union (fv f1) (fv f2)
	fv (Equi f1 f2) = union (fv f1) (fv f2)
	fv (All x f) = [y | y <- fv f, x /= y]
	fv (Ex x f) = [y | y <- fv f, x /= y]