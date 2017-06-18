(*
Practica04.

Equipo:
	García Argueta Jaime Daniel. 		No cta: 322104739.	      E-mail: jaimegarciaargueta@ciencias.unam.mx
	Flores Gonzalez Luis Brandon. 		No cta: 312218342.		  E-mail:
	iluis@ciencias.unam.mx
*)

(*Definición de los numeros naturales en binario por cero, pares e impares.*)
Inductive BinarioNat : Set := 
| cero : BinarioNat
| doble : (BinarioNat -> BinarioNat)
| doble_mu : (BinarioNat -> BinarioNat).

(*-----------------------Funciones Brindadas.--------------------------------------*)

(*Funcion suma de naturales, ésta función fué tomada del archivo Natutrales.v que se proporciono.*)
Fixpoint sumaNat (x y : nat) : nat :=
match x with
| 0 => y
| S n => S (sumaNat n y)
end.

(*---------------------------------------------------------------------------------*)

(*Función sucesor en BinarioNat.*)
Fixpoint sucesor (b : BinarioNat) : BinarioNat :=
match b with
| cero => doble_mu cero
| doble d => doble_mu d
| doble_mu d => doble (sucesor d)
end.

(*Función binANat que devuelve la representación numerica del parametro BinarioNat.*)
Fixpoint binANat (b : BinarioNat) : nat :=
match b with
| cero => 0
| doble d => (sumaNat (binANat d) (binANat d))
| doble_mu d => S (sumaNat (binANat d) (binANat d))
end.

(*Función natABin que devuelve la representación en natABin del natural pasado como parametro.*)
Fixpoint natABin (n : nat) : BinarioNat :=
match n with
| 0 => cero
| S m => sucesor (natABin m)
end.
