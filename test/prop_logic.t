;; -*- mode: prog; -*-

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
