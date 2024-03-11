;; -*- mode: prog; -*-

Test parsing

  $ atp intro -dump-ast "a"
  Var "a"

  $ atp intro -dump-ast "1 + a"
  Add (Const 1, Var "a")

  $ atp intro -dump-ast "(0 * x + 1) * 3 + 12"
  Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)

Test simplification

  $ atp intro "a"
  a
  a

  $ atp intro "1 + a"
  (1 + a)
  (1 + a)

  $ atp intro "(0 * x + 1) * 3 + 12"
  ((((0 * x) + 1) * 3) + 12)
  15

  $ cat >test.txt <<EOF
  > (0 * x + 1) * 3 + 12
  > EOF

  $ atp intro <test.txt
  ((((0 * x) + 1) * 3) + 12)
  15
