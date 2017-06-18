{-
- Flores González Luis Brandon, 312218342, iluis@ciencias.unam.mx
-}

module Semanal1 where

	import LProp

	--Se agrega el tipo Estado que es una lista de VarP, es decir, en esta implementación se
	--usa una lista de enteros para representarlos.
	type Estado = [VarP]
	type Sustitucion = (VarP, Prop)

	--Función recursiva que devuelva la interpretacion de una fórmula dado un estado. Si dentro
	--de la lista(Estado) aparece la variable al igual que en la proposición esta tomara valor
	--de verdadero.
	interp :: Estado -> Prop -> Bool
	interp _ Bot = False
	interp _ Top = True
	-- La función `elem` regresa True si se encuentra la variable(entero) en la lista x.
	interp x (P p1) = (p1 `elem` x)
	interp x (And p1 p2) = ((interp x p1) && (interp x p2))
	interp x (Or p1 p2) = ((interp x p1) || (interp x p2))
	--Sabemos que ((p1) -> (p2)) es equivalente a (~p1 v p2)
	interp x (Impl p1 p2) = (interp x (Or (Neg p1) p2))
	--Sabemos que ((p1) <-> (p2)) es equivalente a (((p1) -> (p2)) v ((p2) -> (p1)))
	interp x (Syss p1 p2) = (interp x (Or (Impl p1 p2) (Impl p2 p1)))
	interp x (Neg p1) = not (interp x p1)

	--Función recursiva que devuelve la interpretación de un conjunto de formulas dado
	--un estado. Es decir debe cumplir con el estado cada elemento(Proposición) de la
	--lista, por este motivo se usa un AND. Ya que si el estado no se cumple para alguna
	--formula del conjunto entonces la interpretación del conjunto sera FALSE.
	interpG :: Estado -> [Prop] -> Bool
	interpG _ [] = True
	interpG e (x:xs) = (interpG e xs) && (interp e x) 

	--Función recursiva la cual recibe una formula, una sustitución y devuelve el resultado 
	--de la sustitución requerida. Ademas se hace uso de guardas para la toma de decisiones.
	sustituye :: Prop -> Sustitucion -> Prop
	sustituye Bot _ = Bot
	sustituye Top _ = Top	
	--Si la proposicion es diferente al primer valor de la dupla, entonces va regresar la
	--misma proposicion. En otro caso, regresara el segundo valor de la dupla.
	sustituye (P p1) (s1, s2) 
		| ((P p1) /= (P s1)) = (P p1)
		| otherwise = s2
	sustituye (And p1 p2) (s1, s2) = And (sustituye p1 (s1, s2)) (sustituye p2 (s1, s2))
	sustituye (Or p1 p2) (s1, s2) = Or (sustituye p1 (s1, s2)) (sustituye p2 (s1, s2))
	sustituye (Impl p1 p2) (s1, s2) = Impl (sustituye p1 (s1, s2)) (sustituye p2 (s1, s2))
	sustituye (Syss p1 p2) (s1, s2) = Syss (sustituye p1 (s1, s2)) (sustituye p2 (s1, s2))
	sustituye (Neg p1) (s1, s2) = Neg (sustituye p1 (s1, s2))

	