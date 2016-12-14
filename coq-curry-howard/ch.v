(* Curry Howard isomorphism:relating logic and programming 
Proof ⇆ Programs
Propositions ⇆ Types 
(((Cut elimination ⇆ Execution))) *)

(* To download coq/coqIDE : https://coq.inria.fr/download *)

(* definition by proof/tactics *)
Definition or_comm : forall P Q, P \/ Q -> Q \/ P.
Proof.
(* introduce the universally quantified variables
   and the assumptions.
   Give them whatever name you want *)
intros A B H.
case H.
- intro HA.
  right.
  exact HA.
- intro HB.
  left.
  exact HB.
Qed.

(* same lemma, proved with more automation *)
(* note that a Lemma is just a particular case of definition *)
Lemma or_comm2 : forall P Q, P \/ Q -> Q \/ P.
Proof.
tauto.
Qed.

(* or is an algebraic datatype.
  Read "Inductive" as "datatype" *)
Print or.

(* our lemmas build terms *)
(* by pattern matching (from the "case" tactic *)
Print or_comm.

(* or by function application *)
Print or_comm2.

(* an induction principle is a higher-order function *)
Print or_ind.

(* definition by term *)
Definition or_comm3 := fun (P Q : Prop) (H : P \/ Q) =>
or_ind (fun H0 : P => or_intror H0) (fun H0 : Q => or_introl H0) H.

(* or_comm3 has the same type/ proves the same statement as the previous ones *)
Check or_comm3.

(* definition by proof with bits of terms *)
Lemma or_comm4 : forall P Q, P \/ Q -> Q \/ P.
Proof.
intros P Q  H.
(* the proof term will be the application of or_ind 
   to some yet undefined arguments *)
refine (or_ind _ _ H). 
- tauto. (* instantiate the 1st missing argument *)
- tauto. (* instantiate the 2nd missing argument *)
Qed.

(* We can extract code to Ocaml! *)
Extraction or_comm.

(* or Haskell! *)
Extraction Language Haskell.
Extraction or_comm.

(* But the definition is emtpy: our lemmas/definitions
are defined on type "Prop", which has no computational content.
Let's do the proof again on type "Type".
(Yes, "Type" is a type, how confusing!) *)

(* let's see how the logical implication "->" corresponds
to the functional arrow type "->"
(which is why coq uses "->" and not "=>") *)
Lemma id : forall A, A -> A.
Proof.
intro A.
intro HA.
exact HA.
Qed.

(* id A is the identity function of type A -> A *)
Print id.

(* indeed *)
Extraction id.

(* disjunction ⇆ sum type *)
(* "+" is the disjunction over "Type", as well as
  "\/" is the disjunction over "Prop" *)
Lemma or_comm_constr P Q : (P + Q) -> (Q + P).
Proof.
(* the same proof works! (this is not always the case *)
intros H.
case H.
- intro HA.
  right.
  exact HA.
- intro HB.
  left.
  exact HB.
Qed.

Print or_comm_constr.

(* add "Recursive" if you want to recursively extract all the definitions *)
(* see that the logical disjunction (\/) is extracted as a 
   sum type *)
Recursive Extraction or_comm_constr.

(* Homework (not done during the session) *)
(* Replace the cheated proofs (Admitted) with a proper proof *)



(* conjunction ⇆ product type *)
(* "*" is the conjunction (/\) over "Type" *)

(* this can be either read as "P and Q implies P" 
or "from something of product type P x Q, I can produce 
   something of type P" *)
Lemma fst P Q : P * Q -> P.
Proof.
(* tauto is sufficient, but you can try a manual proof using 
   the "case" tactic *)
Admitted.

(* see that conjunction extracted as a cartesian product type *)
Recursive Extraction fst.

(* implication elimination (modus ponens) ⇆	application *)
Lemma modus_ponens P Q : P -> (P -> Q) -> Q.
Proof.
(* can be proven using:
  tauto 
  or the (exact ...) tactic
  or the (apply ...) tactic *)
Admitted.

(* the modus_ponens lemma corresponds to the composition function *)
Recursive Extraction modus_ponens.

(* Further reading:
For the Haskell users interested in the Curry-Howard isomorphisme,
there is nice introduction on how to translate basic propositional logic into Haskell programs:
https://en.wikibooks.org/wiki/Haskell/The_Curry%E2%80%93Howard_isomorphism
*)
