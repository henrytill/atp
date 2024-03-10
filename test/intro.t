;; -*- mode: prog; -*-

Test parsing

  $ echo "a" | atp intro -dump-ast
  Var "a"

  $ echo "1 + a" | atp intro -dump-ast
  Add (Const 1, Var "a")

  $ echo "(0 * x + 1) * 3 + 12" | atp intro -dump-ast
  Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)

Test simplification

  $ echo "a" | atp intro
  a
  a

  $ echo "1 + a" | atp intro
  (1 + a)
  (1 + a)

  $ echo "(0 * x + 1) * 3 + 12" | atp intro
  ((((0 * x) + 1) * 3) + 12)
  15

  $ cat >test.txt <<EOF
  > (0 * x + 1) * 3 + 12
  > EOF

  $ atp intro <test.txt
  ((((0 * x) + 1) * 3) + 12)
  15
