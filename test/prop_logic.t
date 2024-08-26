Set up environment

  $ . "${TESTDIR}/setup.sh"

Test parsing

  $ atp prop-logic -dump-ast "p \/ q ==> r"
  (Imp (Or (Atom "p", Atom "q"), Atom "r"))

  $ atp prop-logic -dump-ast "p ==> q /\ ~ r \/ s"
  (Imp (Atom "p", Or (And (Atom "q", Not (Atom "r")), Atom "s")))

  $ atp prop-logic -dump-ast "p /\ q /\ r"
  (And (Atom "p", And (Atom "q", Atom "r")))

  $ atp prop-logic -dump-ast "p ==> q ==> r"
  (Imp (Atom "p", Imp (Atom "q", Atom "r")))

Test printing

  $ atp prop-logic "p \/ q ==> r"
  ((p \/ q) ==> r)

  $ atp prop-logic "p ==> q /\ ~ r \/ s"
  (p ==> ((q /\ (~ r)) \/ s))

  $ atp prop-logic "p /\ q /\ r"
  (p /\ (q /\ r))

  $ atp prop-logic "p ==> q ==> r"
  (p ==> (q ==> r))

Test printing truth table

  $ atp prop-logic -dump-truthtable "p /\ q ==> q /\ r"
  p     q     r     | formula
  ---------------------------
  false false false | true  
  false false true  | true  
  false true  false | true  
  false true  true  | true  
  true  false false | true  
  true  false true  | true  
  true  true  false | false 
  true  true  true  | true  
  ---------------------------

  $ atp prop-logic -dump-ast -dump-truthtable "p /\ q ==> q /\ r"
  (Imp (And (Atom "p", Atom "q"), And (Atom "q", Atom "r")))
  p     q     r     | formula
  ---------------------------
  false false false | true  
  false false true  | true  
  false true  false | true  
  false true  true  | true  
  true  false false | true  
  true  false true  | true  
  true  true  false | false 
  true  true  true  | true  
  ---------------------------

  $ atp prop-logic -dump-truthtable "((p ==> q) ==> p) ==> p"
  p     q     | formula
  ---------------------
  false false | true  
  false true  | true  
  true  false | true  
  true  true  | true  
  ---------------------

  $ atp prop-logic -dump-truthtable "p /\ ~p"
  p     | formula
  ---------------
  false | false 
  true  | false 
  ---------------

  $ atp prop-logic -dump-truthtable "(p \/ q /\ r) /\ (~p \/ ~r)"
  p     q     r     | formula
  ---------------------------
  false false false | false 
  false false true  | false 
  false true  false | false 
  false true  true  | true  
  true  false false | true  
  true  false true  | false 
  true  true  false | true  
  true  true  true  | false 
  ---------------------------


# Local Variables:
# mode: prog
# tab-width: 2
# eval: (whitespace-mode 0)
# End:
