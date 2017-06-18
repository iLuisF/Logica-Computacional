{-
 - Garcia Argueta Jaime Daniel.   No cta: 312104739.  email: jaimegarciaargueta@ciencias.unam.mx
-}

module Semanal1 where

        import LProp

       --Los estados son representados con listas de enteros, donde una variable proposicional es verdadera si es que esta en la lista del estado.
	type Estado = [Int]

	--Tipo de sustitucion textual.
	type Sustitucion = (VarP,Prop)

	--Función interp que evalua una formula proposicional de acuerdo con el estado pasado como parametro.
	interp :: Estado -> Prop -> Bool
	interp _ Top = True
	interp _ Bot = False
	interp l (P x) = listcheck x l
	interp l (Neg p) = not (interp l p)
	interp l (Or p q) = (interp l p) || (interp l q)
	interp l (And p q) = (interp l p) && (interp l q)
	interp l (Impl p q) = (interp l (Neg p)) || (interp l q)
	interp l (Syss p q) = (interp l (Impl p q)) && (interp l (Impl q p))

	--Funcion interpG que evalua si el conjunto de formulas pasado como segundo parametro es satisfacible(devuelve su interpretación) con el estado pasado como primer parametro.
	interpG :: Estado -> [Prop] -> Bool
	interpG _ [] = True
	interpG l (x:xs) = (interp l x) && (interpG l xs)

	--Función sustituye que aplica una sustitucion textual en la proposicion pasada como primer parametro respecto a la sustitucion pasada como segundo parametro.
	sustituye :: Prop -> Sustitucion -> Prop
	sustituye Top _ = Top
	sustituye Bot _ = Bot
	sustituye (P x) (y, p) = if (P x) == (P y) then p else (P x)
	sustituye (Neg q) (y, p) = Neg (sustituye q (y, p))
	sustituye (Or q r) (y, p) = Or (sustituye q (y, p)) (sustituye r (y, p))
	sustituye (And q r) (y, p) = And (sustituye q (y, p)) (sustituye r (y, p))
	sustituye (Impl q r) (y, p) = Impl (sustituye q (y, p)) (sustituye r (y, p))
	sustituye (Syss q r) (y, p) = Syss (sustituye q (y, p)) (sustituye r (y, p))

	--Función estados que devuelve las combinaciones de posibles valores que pueden tener las variables proposicionales presentes en la proposicion pasada como argumento.
	estados :: Prop -> [Estado]
	estados Top = []
	estados Bot = []
	estados p = powerset (vars p)

------------------------------Extra.-------------------------------------------

	--Función estadosConj que devuelve las combinaciones de posibles valores que pueden tomar las variables proposicionales en las proposiciones del conjunto pasado como parametro.
	estadosConj :: [Prop] -> [Estado]
	estadosConj [] = []
	estadosConj x = powerset (vars (andformulas x))  

---------------------------Auxiliares.-----------------------------------------

	--Función que verifica si el indice entero de la variable proposicional pasado como primer parametro esta en la lista pasada como segundo parametro.
	listcheck :: VarP -> [VarP] -> Bool
	listcheck _ [] = False
	listcheck z (x:xs) = if z == x then True else (listcheck z xs)
	
	--Función powerset que devuelve una lista representando al conjunto potencia de la lista asumida como conjunto que se pasa como argumento.
	powerset :: [VarP] -> [Estado]
	powerset [] = [[]]
	powerset (x:xs) = extiende x (powerset xs) 

	--Funcion extiende que esxtiende a la potencia de una lista y:ys asumida como conjunto a el conjunto potencia de la lista [x]++(y:ys).
	extiende :: Int -> [Estado] -> [Estado]
	extiende x [[]] = [[x],[]]
	extiende x (y:ys) = [[x] ++ y] ++ [y] ++ (extiende x ys)

	--Función andformulas que dado un conjunto de formulas devuelve la conjuncion de todas ellas.
	andformulas :: [Prop] -> Prop
	andformulas [p] = p
	andformulas (x:xs) = And x (andformulas xs)

