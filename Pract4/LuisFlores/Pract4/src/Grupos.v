(*
Logica Computacional 2016-2
Teoría de grupos en Coq
Profesor: Dr. Favio E. Miranda Perea
Ayudante: Susana H. Martin Lunas
Laboratorio: Fernando A. Galicia Mendoza
*)

(*Para la reescritura de terminos de un tipo Set*)
Require Import Setoid.

(*Axiomas minimos para tener un grupo*)

(*Necesitamos un conjunto*)
Hypothesis G : Set.

(*Operacion sobre G*)
Hypothesis prod : G -> G -> G.

(*Elemento identidad*)
Hypothesis id : G.

(*Inverso*)
Hypothesis i : G -> G.

Infix "&" := prod (at level 40).

(*Axiomas de grupo*)
Axiom asociatividad : forall (x y z : G), x & (y & z) = (x & y) & z.
Axiom identidad_izq : forall (x : G), id & x = x.
Axiom inverso_izq : forall (x : G), (i x) & x = id.

Lemma involucion : forall (x : G), x & x = x -> x = id.
Proof.
intro x.
intro.
rewrite <- identidad_izq with x.
rewrite <- inverso_izq with x.
rewrite <- asociatividad.
rewrite H.
reflexivity.
Qed.

Lemma inverso_der: forall (x : G), x & (i x) = id.
Proof.
intro x.
rewrite <- inverso_izq with x.
rewrite <- identidad_izq with (i x).
rewrite <- asociatividad.
rewrite inverso_izq with x.
rewrite identidad_izq with id.
apply involucion.
rewrite identidad_izq.
rewrite asociatividad.
rewrite <- asociatividad with x (i x) x.
rewrite inverso_izq.
rewrite <- asociatividad.
rewrite identidad_izq.
reflexivity.
Qed.

Lemma identidad_der: forall (x : G), x & id = x.
Proof.
intro x.
rewrite <- inverso_izq with x.
rewrite asociatividad.
rewrite inverso_der with x.
rewrite identidad_izq.
reflexivity.
Qed.

Lemma cancelacion_izq: forall (x y z : G), x & y = x & z -> y = z.
Proof.
intro x.
intro y.
intro z.
intro.
rewrite <- identidad_izq with y.
rewrite <- identidad_izq with z.
rewrite <- inverso_izq with x.
rewrite <- asociatividad.
rewrite <- asociatividad with (i x) x z.
rewrite H.
reflexivity.
Qed.

(*Tarea*)

Lemma cancelacion_der: forall (x y z : G), y & x = z & x -> y = z.
intros.
rewrite <- identidad_der with y.
rewrite <- identidad_der with z.
rewrite <- inverso_der with x.
rewrite asociatividad.
rewrite asociatividad.
rewrite H.
trivial.
Qed.

Lemma inv_izq_id: forall (x y : G), (i x) & (x & y) = y.
intros.
rewrite asociatividad.
rewrite inverso_izq.
rewrite identidad_izq.
trivial.
Qed.

Lemma unicidad_inv: forall (x y : G), x & y = id -> x = i y.
intros.
apply cancelacion_der with y.
rewrite H.
rewrite inverso_izq.
trivial.
Qed.

Lemma invinv_es_id: forall (x : G), i (i x) = x.
intros.
apply cancelacion_der with (i x).
rewrite inverso_der.
rewrite inverso_izq.
trivial.
Qed.

Lemma id_der_inv : forall (x : G), x & id = i (i x).
intros.
rewrite invinv_es_id.
rewrite identidad_der.
trivial.
Qed.

(*Segundo grupo*)

(*Necesitamos un conjunto*)
Hypothesis G2 : Set.

(*Operacion sobre G*)
Hypothesis prod2 : G2 -> G2 -> G2.

(*Elemento identidad*)
Hypothesis id2 : G2.

(*Inverso*)
Hypothesis i2 : G2 -> G2.

(*Operacion del segundo grupo*)
Infix "&>" := prod2 (at level 40).

(*Axiomas de grupo*)
Axiom asociatividad2 : forall (x y z : G2), x &> (y &> z) = (x &> y) &> z.
Axiom identidad_izq2 : forall (x : G2), id2 &> x = x.
Axiom inverso_izq2 : forall (x : G2), (i2 x) &> x = id2.

(*Definicion de homomorfismo de grupos*)
Definition morfismo (f : G -> G2) := forall (x y : G), 
f (x & y) = (f x) &> (f y).


(*..........Propiedades análogas para el grupo G2.......*)
Lemma involucion2 : forall (x : G2), x &> x = x -> x = id2.
Proof.
intro x.
intro.
rewrite <- identidad_izq2 with x.
rewrite <- inverso_izq2 with x.
rewrite <- asociatividad2.
rewrite H.
reflexivity.
Qed.

Lemma inverso_der2: forall (x : G2), x &> (i2 x) = id2.
Proof.
intro x.
rewrite <- inverso_izq2 with x.
rewrite <- identidad_izq2 with (i2 x).
rewrite <- asociatividad2.
rewrite inverso_izq2 with x.
rewrite identidad_izq2 with id2.
apply involucion2.
rewrite identidad_izq2.
rewrite asociatividad2.
rewrite <- asociatividad2 with x (i2 x) x.
rewrite inverso_izq2.
rewrite <- asociatividad2.
rewrite identidad_izq2.
reflexivity.
Qed.

Lemma identidad_der2: forall (x : G2), x &> id2 = x.
Proof.
intro x.
rewrite <- inverso_izq2 with x.
rewrite asociatividad2.
rewrite inverso_der2 with x.
rewrite identidad_izq2.
reflexivity.
Qed.

Lemma cancelacion_der2: forall (x y z : G2), y &> x = z &> x -> y = z.
intros.
rewrite <- identidad_der2 with y.
rewrite <- identidad_der2 with z.
rewrite <- inverso_der2 with x.
rewrite asociatividad2.
rewrite asociatividad2.
rewrite H.
trivial.
Qed.
(*..................................................*)

Lemma id_bajo_mor : forall (f : G -> G2), morfismo f -> f id = id2.
intros.
apply cancelacion_der2 with (f id).
rewrite <- H.
rewrite identidad_izq.
rewrite identidad_izq2.
trivial.
Qed.
