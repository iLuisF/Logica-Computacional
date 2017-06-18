{-
 - García Argueta Jaime Daniel.   No Cta: 312104739.	e-mail: jaimegarciaargueta@ciencias.unam.mx
 -}

module Semanal2 where

	import Data.List
	import LPO


	--Tipo de dato que representa a una sustitución en la logica de primer orden. Nombre es la variable que será sustituida y Term es el término a que reemplazara a la variable.
	type Sustitucion = (Nombre,Term)

	--Tipo de dato que representa una lista de sustituciònes.
	type Subst = [Sustitucion]

	--Dato que representa de manéra alternativa a los cuantificadores de la lógica de primer órden.
	data Cuantificador = ParaTodo | Existe deriving(Show)

	--Función bv que devuelve el conjunto(lista) con los nombre de las variables ligadas presentas en esta formula.
	bv :: Form -> [Nombre]
	bv (Neg x) = bv x
	bv (Conj x y) = union (bv x) (bv y)
	bv (Disy x y) = union (bv x) (bv y)
	bv (Imp x y) = union (bv x) (bv y)
	bv (Equi x y) = union (bv x) (bv y)
	bv (All s f) = union [s] (bv f)
	bv (Ex s f) = union [s] (bv f)
	bv z = []

	--Función verifSus que vèrifica si una lista de sustituciones es vàlida con respecto a la condiciòn de que todas la variables a sustituir deben de ser distintas y la condiciòn de que un termino de los que se sustituiràn no puede ser igual a una variable que vaya a ser sustituida.  
	verifSus :: Subst -> Bool
	verifSus x = (verifVar x) && (verifVector x) where

	verifVar x = (names x) == (namesWOutRep x)

	--Funciòn apSubT que sòlo es una fachada para la funciòn apSubTaux que aplica una lista de sustituciònes a un termino en caso de que la sustituciòn sea valida, en caso contrario lanza un mensaje de error.
	apSubT :: Term -> Subst -> Term
	apSubT t l = if (verifSus l) then (apSubTaux t l) else error "Sustituciòn invàlida." 

	--Funciòn apsubF que es una fachada para la funciòn auxiliar apsubFaux que aplica una lista de substituciònes a una formula en caso de que la sustitucion sea valida y se pueda realizar en caso contrario lanza un mensaje de error.
	apsubF :: Form -> Subst -> Form
	apsubF x l = if (verifSus l) then (apsubFaux x l) else error "Sustituciòn inválida."

	--Función subformCuan que obtiene una lista con todas las subformulas cuantificadas de esta formula.
	subformCuan :: Form -> [Form]
	subformCuan (Neg x) = subformCuan x
	subformCuan (Conj x y) = (subformCuan x) ++ (subformCuan y) 
	subformCuan (Disy x y) = (subformCuan x) ++ (subformCuan y)
	subformCuan (Imp x y) = (subformCuan x) ++ (subformCuan y)
	subformCuan (Equi x y) = (subformCuan x) ++ (subformCuan y)
	subformCuan (All s x) = [All s x] ++ (subformCuan x)
	subformCuan (Ex s x) = [Ex s x] ++ (subformCuan x)
	subformCuan x = []

	--Función alcance que obtiene la lista de 3-tuplas que representan el alcance de cada cuantificador.
	alcance :: Form -> [(Cuantificador, Nombre, Form)]
	alcance x = map (tuplaCuan) (subformCuan x)

-----------------------------Auxiliares.------------------------------------

	--Función auxiliar names que devuelve la lista con los nombres(variables a sustituir) que figuran en esta lista de sustituciònes(con repeticiòn de nombres si un nombre aparece màs de una vez).
	names :: Subst -> [Nombre]
	names [] = []
	names (x:xs) = [fst x] ++ (names xs)

	--Función auxiliar namesWOutRep que hace lo mismo que la funciòn names pero no repite nombres aunque aparezcan màs de una vez.
	namesWOutRep :: Subst -> [Nombre]
	namesWOutRep [] = []
	namesWOutRep (x:xs) = union [fst x] (namesWOutRep xs)	

	--Función auxiliar terms que devuelve la lista con los terminos que figuran en la segunda entrada de una lista de sustituciones(si un elemento figura mas de una vez sòlo se considera una en la lista.).
	terms :: Subst -> [Term]
	terms [] = []
	terms (x:xs) = union [snd x] (terms xs)	

	--Funciòn auxiliar verifVector que se encarga de verificar si los nombres de variables en esta lìsta de sustituciònes no aparecen còmo tèrminos que sustituiràn a variables.
	verifVector :: Subst -> Bool
	verifVector [] = True
	verifVector (x:xs) = (verifV (fst x) (snd x)) && (verifVector xs)

	--Funciòn auxiliar verifV que verifica si una variable(un nombre) es distinta a un termino dado.
	verifV :: Nombre -> Term -> Bool
	verifV x (V y) = x /= y
	verifV x (F _ _) = True

	--Funciòn auxiliar apSubTaux que aplica una lista de sustituciònes a un termino en caso de que la sustituciòn sea valida, en caso contrario lanza un mensaje de error.
	apSubTaux :: Term -> Subst -> Term
	apSubTaux t [] = t 
	apSubTaux (V z) (x:xs) = if (z == (fst x)) then snd x else (apSubT (V z) xs)
	apSubTaux (F s []) _ = F s []
	apSubTaux (F s r) l = F s (apSubTlist r l) where

	apSubTlist [] _ = []
	apSubTlist (x:xs) l = [apSubT x l] ++ (apSubTlist xs l)

	--Funciòn auxiliar apsusFaux que se encarga de realmente aplicar una lista de sustituciònes a una formula en caso de que la sustitucion sea valida y se pueda realizar en caso contrario lanza un mensaje de error.
	apsubFaux :: Form -> Subst -> Form
	apsubFaux TrueF _ = TrueF
	apsubFaux FalseF _ = FalseF
	apsubFaux (Pr s r) l = Pr s (apsubFlist r l)
	apsubFaux (Eq x y) l = Eq (apSubT x l) (apSubT y l)
	apsubFaux (Neg x) l = Neg (apsubF x l)
	apsubFaux (Conj x y) l = Conj (apsubF x l) (apsubF y l)
	apsubFaux (Disy x y) l = Disy (apsubF x l) (apsubF y l)
	apsubFaux (Imp x y) l = Imp (apsubF x l) (apsubF y l)
	apsubFaux (Equi x y) l = Equi (apsubF x l) (apsubF y l)
	apsubFaux (All s x) l = if ((notElem s (namesWOutRep l)) && (notElem (V s) (terms l))) then (All s (apsubFaux x l)) else error "No es posible  hacer la sustituciòn."
	apsubFaux (Ex s x) l = if ((notElem s (namesWOutRep l)) && (notElem (V s) (terms l))) then (Ex s (apsubFaux x l)) else error "No es posible hacer la sustituciòn." where

	apsubFlist [] _ = []
	apsubFlist (x:xs) l = [apSubT x l] ++ (apsubFlist xs l)
	
	--Función auxiliar tuplaCuan que dada una fórmula cuantificada devuelve la 3-tupla que representa el alcance de su cuantificador.
	tuplaCuan :: Form -> (Cuantificador, Nombre, Form)
	tuplaCuan (All s x) = (ParaTodo, s, x)
	tuplaCuan (Ex s x) = (Existe, s, x)
