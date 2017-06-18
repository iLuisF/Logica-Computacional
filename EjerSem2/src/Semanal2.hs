module Semanal2 where 

	import LPO
	import Data.List
	type Subst = [(Nombre,Term)]
	data Cuantificador = ParaTodo | Existe deriving(Show)
	
	--Funcion que devuelve las variables ligadas de una formula
	bv :: Form -> [Nombre]
	bv TrueF = []
	bv FalseF = []
	bv (Pr p lt) = concat (map varT lt)
	bv (Eq t1 t2) = union (varT t1) (varT t2)
	bv (Neg f) = bv f
	bv (Conj f1 f2) = union (bv f1) (bv f2)
	bv (Disy f1 f2) = union (bv f1) (bv f2)
	bv (Imp f1 f2) = union (bv f1) (bv f2)
	bv (Equi f1 f2) = union (bv f1) (bv f2)
	bv (All x f) = [y | y <- bv f, x == y]
	bv (Ex x f) = [y | y <- bv f, x == y]	

	--Función recursiva que obtiene todas las subformulas cuantificadas de una formula.
	subformCuan :: Form -> [Form]
	subformCuan TrueF = []
	subformCuan FalseF = []
	subformCuan (Pr p lt) = [(Pr p lt)]
	subformCuan (Eq t1 t2) = [(Eq t1 t2)] 
	subformCuan (Neg f) = subformCuan f
	subformCuan (Conj f1 f2) = (subformCuan f1) ++ (subformCuan f2)
	subformCuan (Disy f1 f2) = (subformCuan f1) ++ (subformCuan f2)
	subformCuan (Imp f1 f2) = (subformCuan f1) ++ (subformCuan f2)
	subformCuan (Equi f1 f2) = (subformCuan f1) ++ (subformCuan f2)
	subformCuan (All x f) = [All x f] ++ (subformCuan f)
	subformCuan (Ex x f) = [Ex x f] ++ (subformCuan f)


	--Función alcance que obtiene la lista de 3-tuplas que representan el alcance de cada 
	--cuantificador.
	alcance :: Form -> [(Cuantificador, Nombre, Form)]	
	alcance TrueF = []
	alcance FalseF = []
	alcance (Neg f) = alcance f
	alcance (Conj f1 f2) = (alcance f1) ++ (alcance f2)
	alcance (Disy f1 f2) = (alcance f1) ++ (alcance f2)
	alcance (Imp f1 f2) = (alcance f1) ++ (alcance f2)
	alcance (Equi f1 f2) = (alcance f1) ++ (alcance f2)
	alcance (All x f) = [(ParaTodo, x, f)] ++ (alcance f)
	alcance (Ex x f) = [(Existe, x, f)] ++ (alcance f)	
	alcance x = []

	--Función recursiva que determina si una sustitución es válida.
	verifSus :: Subst -> Bool
	verifSus x = ((names x) == (namesWOutRep x))

	--Función auxiliar names que devuelve la lista con los nombres(variables a sustituir) 
	--que figuran en esta lista de sustituciònes(con repeticiòn de nombres si un nombre aparece màs de una vez).
	names :: Subst -> [Nombre]
	names [] = []
	names (x:xs) = [fst x] ++ (names xs)

	--Función auxiliar namesWOutRep que hace lo mismo que la funciòn names pero no repite nombres aunque aparezcan màs de una vez.
	namesWOutRep :: Subst -> [Nombre]
	namesWOutRep [] = []
	namesWOutRep (x:xs) = union [fst x] (namesWOutRep xs)	