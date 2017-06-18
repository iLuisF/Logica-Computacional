{- Equipo:
 - Garcìa Argueta Jaime Daniel.    No cta: 312104739.      Email: jaimegarciaargueta@ciencias.unam.mx
 - Flores González Luis Brandon.   No cta: 312218342	   Email: iluis@ciencias.unam.mx
 -}

module Practica3 where

	import Data.List
	import LPO
	import LPOSemantica
	import Semanal2

	--Funciòn que simplifica una lista de sustituciònes eliminando las del tipo (x, V x) (x:=x).
	simpSus :: Subst -> Subst
	simpSus [] = []
	simpSus (x:xs) = if V (fst x) == (snd x) then simpSus xs else [x] ++ (simpSus xs)

	--Funciòn que devuelve la composiciòn de las dos funciones dadas (primero se aplica la del primer parametro y despues la del segundo parametro).
	compSus :: Subst -> Subst -> Subst
	compSus [] l = l
	compSus l [] = l
	compSus l1 l2 = simpSus ((compSusAux1 l1 l2) ++ (compSusAux2 l1 l2)) where
		--Se encarga de el dominio de la primera sustituciòn unicamente.
	compSusAux1 [x] l = [(fst x, apSubT (snd x) l)]
	compSusAux1 (x:xs) l = [(fst x, apSubT (snd x) l)] ++ (compSusAux1 xs l)
		--Se encarga de las variables que solo son sustituidas en la segunda sustituciòn.
	compSusAux2 l [x] = if elem (fst x) (namesWOutRep l) then [] else [x]
	compSusAux2 l (x:xs) = if elem (fst x) (namesWOutRep l) then compSusAux2 l xs else [x] ++ (compSusAux2 l xs)

	--Funciòn que obtiene el unificador mas general de dos terminos si es que este existe y si no devuelve la lista vacìa.
	unifica :: Term -> Term -> [Subst]
	unifica (V x) (V y) = [[(x,V y)]]
	unifica (V x) (F f l) = if verifOcurr x (F f l) then [] else [[(x, F f l)]]
	unifica (F g k) (V y) = if verifOcurr y (F g k) then [] else [[(y, F g k)]]
	unifica (F g k) (F f l) = if g /= f then [] else unificaListas k l     where
		--Unifica las listas de terminos de una funcion termino a termino y luego las compone.
	unificaListas [] [] = [[]]
	unificaListas (x:xs) (y:yr) = comp (unifica x y) (unificaListas (apSusTList xs (unifica x y)) (apSusTList yr (unifica x y)))

	--Funciòn que obtiene el unificador màs general de un conjunto de terminos si es que este existe, si no entonces devuelve la lista vacìa.
	unificaConj :: [Term] -> [Subst]
	unificaConj [] = [[]]
	unificaConj [t] = [[]]
	unificaConj (x:(y:yr)) = comp (unifica x y) (unificaConj (apSusTList (y:yr) (unifica x y)))

	--Funciòn que obtiene el unificador màs general de dos literales, en caso de que no sea posible unificar es regresada la lista vacìa.
	unificaLit :: Form -> Form -> [Subst]
	unificaLit TrueF TrueF = [[]]
	unificaLit FalseF FalseF = [[]]
	unificaLit (Eq t1 t2) (Eq s1 s2) = comp (unificaConj [t1, s1]) (unificaConj (apSusTList [t2, s2] (unificaConj [t1, s1])))
	unificaLit (Pr p k) (Pr r l) = if p /= r then [] else unificaListas k l     where
	unificaLit (Neg f1) (Neg f2) = unificaLit f1 f2
	unificaLit l1 l2 = []

--------------------------------Auxiliares.------------------------------------

	--Funciòn que devuelve verdadero si el nombre de la variable dado ocurre en el termino dado.
	verifOcurr :: Nombre -> Term -> Bool
	verifOcurr s (V x) = (s == x)
	verifOcurr s (F f l) = verifOcurrList s l     where
		--Lo verifica en la lista de terminos de una funciòn.
	verifOcurrList s [] = False
	verifOcurrList s (x:xs) = (verifOcurr s x) || (verifOcurrList s xs)
	

	--Funciòn que aplica una sustituciòn(en forma de lista de lista de sustituciones) a una lista de terminos.
	apSusTList :: [Term] -> [Subst] -> [Term]
	apSusTList [] _ = []
	apSusTList (x:xs) [s] = [apSubT x s] ++ (apSusTList xs [s])

	--Funcion que realiza una composiciòn de sustituciònes excepto que en esta composiciòn si alguna de las dos sustituciones es vacia el resultado tambien serà la vacìa.
	comp :: [Subst] -> [Subst] -> [Subst]
	comp [] _ = []
	comp _ [] = []
	comp [k] [l] = [compSus k l]
