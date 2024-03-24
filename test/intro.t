;; -*- mode: prog; -*-

Test parsing

  $ atp intro -dump-ast "a"
  Var "a"

  $ atp intro -dump-ast "42"
  Const 42

  $ atp intro -dump-ast "42 + 42"
  Add (Const 42, Const 42)

  $ atp intro -dump-ast "42 * 42"
  Mul (Const 42, Const 42)

  $ atp intro -dump-ast "2 ^ 3"
  Exp (Const 2, Const 3)

  $ atp intro -dump-ast "x - - - x"
  Sub (Var "x", Neg (Neg (Var "x")))

  $ atp intro -dump-ast "2 * x + y"
  Add (Mul (Const 2, Var "x"), Var "y")

  $ atp intro -dump-ast "(0 * x + 1) * 3 + 12"
  Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12)

Test precedence

  $ atp intro -dump-ast "1 + 2 * 3"
  Add (Const 1, Mul (Const 2, Const 3))

  $ atp intro -dump-ast "1 * 2 + 3"
  Add (Mul (Const 1, Const 2), Const 3)

  $ atp intro -dump-ast "x + y + z"
  Add (Add (Var "x", Var "y"), Var "z")

  $ atp intro -dump-ast "x - y - z"
  Sub (Sub (Var "x", Var "y"), Var "z")

  $ atp intro -dump-ast "x * y * z"
  Mul (Mul (Var "x", Var "y"), Var "z")

  $ atp intro -dump-ast "x ^ y ^ z"
  Exp (Var "x", Exp (Var "y", Var "z"))

  $ atp intro -dump-ast "(1 + 2) * 3"
  Mul (Add (Const 1, Const 2), Const 3)

  $ atp intro -dump-ast "1 * (2 + 3)"
  Mul (Const 1, Add (Const 2, Const 3))

Test simplification

  $ atp intro "a"
  a
  a

  $ atp intro "1 + a"
  (1 + a)
  (1 + a)

  $ atp intro "0 + x"
  (0 + x)
  x

  $ atp intro "x + 0"
  (x + 0)
  x

  $ atp intro "3 - 2"
  (3 - 2)
  1

  $ atp intro "x - 0"
  (x - 0)
  x

  $ atp intro "x - x"
  (x - x)
  0

  $ atp intro "3 * 4"
  (3 * 4)
  12

  $ atp intro "0 * x"
  (0 * x)
  0

  $ atp intro "x * 0"
  (x * 0)
  0

  $ atp intro "1 * x"
  (1 * x)
  x

  $ atp intro "x * 1"
  (x * 1)
  x

  $ atp intro "2 ^ 3"
  (2 ^ 3)
  8

  $ atp intro "0 ^ 0"
  (0 ^ 0)
  1

  $ atp intro "0 ^ x"
  (0 ^ x)
  0

  $ atp intro "x ^ 0"
  (x ^ 0)
  1

  $ atp intro "1 ^ x"
  (1 ^ x)
  1

  $ atp intro "x ^ 1"
  (x ^ 1)
  x

  $ atp intro "x - - - x"
  (x - - - x)
  0

  $ atp intro "(0 * x + 1) * 3 + 12"
  ((((0 * x) + 1) * 3) + 12)
  15

Misc

  $ cat >test.txt <<EOF
  > (0 * x + 1) * 3 + 12
  > EOF

  $ atp intro <test.txt
  ((((0 * x) + 1) * 3) + 12)
  15

Count number of simplification steps

  $ atp intro -count "a"
  a
  a
  Simplification steps: 2

  $ atp intro -count "1 + 1"
  (1 + 1)
  2
  Simplification steps: 6

  $ atp intro -count "(1 + 2) * (3 + 4)"
  ((1 + 2) * (3 + 4))
  21
  Simplification steps: 14
