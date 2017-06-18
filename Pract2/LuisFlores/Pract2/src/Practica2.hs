{-
 - Equipo:
 - García Argueta Jaime Daniel.  No cta: 312104739.   email: jaimegarciaargueta@ciencias.unam.mx
 - Flores González Luis Brandon. No cta: 312218342. email: iluis@ciencias.unam.mx
 -}

module Practica2 where 

	import LProp
	import Semanal1
	import Data.List

	--Función que devuelve la lista de estados que satisfacen a el conjunto de formulas pasado 
	--como parametro.
	modelosConj :: [Prop] -> [Estado]
	modelosConj [] = error "Cualquier interpretación es modelo del conjunto vacío."
	modelosConj [Top] = error "Cualquier interpretación es modelo de este conjunto."
	modelosConj x = modConjAux x (estadosConj x)

	--Función que decide verdadero si el estado pasado como primer parametro es modelo del
	--conjunto de formulas pasado como segundo parametro.
	satisfenConj :: Estado -> [Prop] -> Bool
	satisfenConj e c = (interpG e c) == True

	--Función que devuelve verdadero si existe una interpretacion que satisfaga a todas las
	--formulas en el conjunto.
	satisfConj :: [Prop] -> Bool
	satisfConj [] = True
	satisfConj [Top] = True
	satisfConj x = (modelosConj x) /= []

	--Función dual de satisfenConj, es decir, devuelve verdadero si el estado pasado como 
	--parametro no es modelo del conjunto de formulas.
	insatisfenConj :: Estado -> [Prop] -> Bool
	insatisfenConj x y = not (satisfenConj x y)

	--Función dual de satisfConj, es decir, devuelve verdadero si el conjunto de formulas
	--no tiene modelo.
	insatisfConj :: [Prop] -> Bool
	insatisfConj x = not (satisfConj x) 

	--Función que dada una formula devuelve una equivalente en forma normal conjuntiva.
	fnc :: Prop -> Prop
	fnc p = fncAux (fnn p)
	
	--Funciòn que recibe como parametro exclusivamente una formula en forma normal conjuntiva(fnc)
	--y devuelve un conjunto(lista) de clausulas representando a la formula en forma normal conjuntista.
	fncConj :: Prop -> [[Prop]]
	fncConj (And p q) = (fncConj p) ++ (fncConj q)
	fncConj p = [clConj p]

	--Funciòn que devuelve True en caso de que la formula en forma normal conjuntiva conjuntista(fncc) 
	--sea satisfacible en el estado pasado como parametro.
	interpFNCC :: Estado -> [[Prop]] -> Bool
	interpFNCC e [c] = interpCl e c
	interpFNCC e (x:xs) = if (not (interpCl e x)) then False else (interpFNCC e xs)

	--Funciòn que devuelve el conjunto de variables presentes en la formula en forma fncc pasada
	--como parametro.
	varsFNCC :: [[Prop]] -> [VarP]
	varsFNCC [c] = varsCl c
	varsFNCC (x:xs) = union (varsCl x) (varsFNCC xs)

	--Funciòn que devuelve una lista con todos los posibles estados de las variables presentes en
	--la formula pasada como parametro(en fncc).
	estadosFNCC :: [[Prop]] -> [Estado]
	estadosFNCC x = estados (fnccToProp x)

	--Funciòn que devuelve una lista con los estados que satisfacen a la formula pasada como 
	--parametro en forma normal conjuntiva conjuntista.
	modelosFNCC :: [[Prop]] -> [Estado]
	modelosFNCC x = modelosConj [(fnccToProp x)]

--------------------Punto extra------------------------------

	--Función que hace la resolución entre una literal y una clausula. Requiere una proposición
	--y una lista de ellas. Ademas usa la función auxiliar negLiteral.
	resLit :: Prop -> [Prop] -> [Prop]
	resLit p [q] = if (negLiteral p q) then [] else [q]
	resLit p (x:xs) = if (negLiteral p x) then xs else [x] ++ (resLit p xs)

	--Función auxiliar recursiva que analiza si una literal es la negación de otra.
	--Pero no cada caso exhaustivamente.
	negLiteral :: Prop -> Prop -> Bool
	negLiteral Top Bot = True
	negLiteral Bot Top = True
	negLiteral Bot (Neg bot) = True
	negLiteral Top (Neg top) = False
	negLiteral (P x) (Neg(P y)) =  if x == y then True else False
	negLiteral (Neg(P x)) (P y) = if x == y then True else False
	negLiteral p (Neg q) = not (negLiteral p q)
	negLiteral (Neg p) q = not (negLiteral p q)
	negLiteral p q = False
	
--------------------Auxiliares.------------------------------
	
	--Función auxiliar que busca en una lista de estados los que satisfacen a la formula pasada 
	--como parametro y devuelve una lista con estos.
	modConjAux :: [Prop] -> [Estado] -> [Estado]
	modConjAux _ [] = []
	modConjAux c (x:xs) = if (interpG x c) then [x]++(modConjAux c xs) else modConjAux c xs 

	--Función auxiliar que dada una formula devuelve una equivalente en forma normal negativa fnn
	--(formula donde no figuran implicaciones ni dobles implicaciones además de que las negaciones 
	--solo se aplican a variables proposicionales).
	fnn :: Prop -> Prop
	fnn Top = Top
	fnn Bot = Bot
	fnn (P x) = (P x)
	fnn (Neg Top) = Bot
	fnn (Neg Bot) = Top
	fnn (Neg (P x)) = Neg (P x)
	fnn (Neg (And p q)) = Or (fnn (Neg p)) (fnn (Neg q))
	fnn (Neg (Or p q)) = And (fnn (Neg p)) (fnn (Neg q))
	fnn (Neg (Impl p q)) = And (fnn p) (fnn (Neg q))
	fnn (Neg (Syss p q)) = fnn (Neg (And (Impl p q) (Impl q p)))
	fnn (And p q) = And (fnn p) (fnn q)
	fnn (Or p q) = Or (fnn p) (fnn q)
	fnn (Impl p q) = Or (fnn (Neg p)) (fnn q)
	fnn (Syss p q) = fnn (And (Impl p q) (Impl q p))

	--Función auxiliar que realiza el trabajo de transformar una formula en exclusivamente forma 
	--normal negativa fnn a una equivalente en forma normal conjuntiva.
	fncAux :: Prop -> Prop
	fncAux Top = Top
	fncAux Bot = Bot
	fncAux (P x) = (P x)
	fncAux (Neg p) = Neg p
	fncAux (And p q) = And (fncAux p) (fncAux q)
	fncAux (Or (And p q) (r)) = And (fncAux (Or p r)) (fncAux (Or q r))
	fncAux (Or p (And q r)) = And (fncAux (Or p q)) (fncAux (Or p r))
	fncAux (Or (Or p q) (Or r s)) =  distribuye (Or (fncAux (Or p q)) (fncAux (Or r s)))
	fncAux (Or p q) = Or p q

	--Funciòn auxiliar construida particularmente para el penultimo caso de la definiciòn de la 
	--funciòn fncAux (el caso: Or (Or p q) (Or r s)). 
	distribuye :: Prop -> Prop
	distribuye (And p q) = And (distribuye p) (distribuye q)
	distribuye (Or (And p q) (r)) = And (distribuye (Or p r)) (distribuye (Or q r))
	distribuye (Or p (And q r)) = And (distribuye (Or p q)) (distribuye (Or p r))
	distribuye (Or (Or p q) (Or r s)) = Or (distribuye (Or p q)) (distribuye (Or r s))
	distribuye (Or p q) = Or p q

	--Funciòn que recibe exclusivamente clausulas y devuelve la clausula en forma de conjunto(lista).
	clConj :: Prop -> [Prop]
	clConj Top = [Top]
	clConj Bot = [Bot]
	clConj (P x) = [P x]
	clConj (Neg p) = [Neg p]
	clConj (Or p q) = (clConj p) ++ (clConj q)

	--Funciòn que devuelve True si la clausula en forma de conjunto pasada como parametro es 
	--satisfacible en el estado pasado como parametro.
	interpCl :: Estado -> [Prop] -> Bool
	interpCl e [l] = interp e l
	interpCl e (x:xs) = if (interp e x) then True else (interpCl e xs)

	--Funciòn auxiliar que devuelve el conjunto de variables de la clausula pasada como parametro.
	varsCl :: [Prop] -> [VarP]
	varsCl [p] = vars p
	varsCl (x:xs) = union (vars x) (varsCl xs)

	--Funciòn auxiliar que transforma una clausula en forma de conjunto a una en forma de proposiciòn.
	clToProp :: [Prop] -> Prop
	clToProp [p] = p
	clToProp (x:xs) = Or (x) (clToProp xs)

	--Funciòn auxiliar que transforma una formula en forma normal conjuntiva conjuntista en su forma
	--de proposiciòn.
	fnccToProp :: [[Prop]] -> Prop
	fnccToProp [c] = clToProp c
	fnccToProp (x:xs) = And (clToProp x) (fnccToProp xs)
