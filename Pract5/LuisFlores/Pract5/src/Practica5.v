(*
Practica 5.
Equipo:
Flores González Luis Brandon
	Jaime Daniel Garcia Argueta	No cta: 312104739	E-mail:jaimegarciaargueta@ciencias.unam.mx
*)

Require Import Classical.
Require Import Utf8.

Theorem ejercicio1a: forall (p q:Prop), (~p /\ ~q) -> ~(p \/ q).
Proof.
intros.
unfold not.
intros.
destruct H.
destruct H0.
apply H.
trivial.
apply H1.
trivial.
Qed.

Theorem ejercicio1c: forall (p q r : Prop), (p → (q ∨ r)) → ((p → q) ∨ (p → r)).
Proof.
intros.
apply NNPP.
unfold not.
intros.
apply not_or_and in H0.
destruct H0.
apply imply_to_and in H0.
apply imply_to_and in H1.
destruct H0.
destruct H1.
apply H in H0.
destruct H0.
apply H2 in H0.
trivial.
apply H3 in H0.
trivial.
Qed.

Theorem ejercicio1e: forall(A : Set) (P Q R : A → Prop) (a : A),
(exists (x:A), P x ∧ Q a) ∧ (forall (x:A), P x → R x) → (Q a ∧ (exists(x:A), (P x ∧ R x))).
Proof.
intros.
destruct H.
destruct H.
destruct H.
split.
trivial.
exists x.
split.
trivial.
apply H0.
trivial.
Qed.

(*Se utilizo tambien el lema de la logica clásica imply_to_or pues no nos salio con solo NNPP.*) 
Theorem ejercicio1g: forall (A : Set) (P Q : A → Prop) (a : A), (P a → (exists (x:A), Q x)) → (exists (x:A), (P a → Q x)).
Proof.
intros.
apply imply_to_or in H.
destruct H.
exists a.
intros.
apply H in H0.
apply NNPP.
unfold not.
intros.
trivial.
destruct H.
exists x.
intros.
trivial.
Qed.

Lemma ejercicio2: forall (a b c f:Prop), ((a -> c /\ b) /\ (~c -> f) /\ (b -> ~f)) -> (a -> ~f).
Proof.
intros.
destruct H.
destruct H1.
apply H in H0.
destruct H0.
apply H2 in H3.
trivial.
Qed.

Lemma ejercicio3: forall (A:Set) (F D M E:A->Prop) (p j:A), ((forall (x:A), F x -> D x) /\ (forall (x:A), M x -> ~E x) /\ F p /\ ~E j) -> ((~forall (x:A), ~D x) \/ (exists (x:A), M x)).
Proof.
intros.
destruct H.
destruct H0.
destruct H1.
left.
unfold not.
intros.
apply H in H1.
apply H3 in H1.
trivial.
Qed.

Lemma ejercicio4: forall (A:Set) (FM EX H C:A -> Prop), ((~exists (x:A), FM x /\ EX x) /\ (forall (x:A), FM x -> H x) /\ (exists (x:A), FM x /\ C x)) -> (exists (x:A), H x /\ ~EX x).
Proof.
intros.
destruct H0.
destruct H1.
destruct H2.
destruct H2.
exists x.
split.
apply H1 in H2.
trivial.
unfold not.
intros.
apply H0.
exists x.
split.
trivial.
trivial.
Qed.

