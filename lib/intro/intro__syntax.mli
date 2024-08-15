(** @canonical Intro.Syntax *)

type t =
  | Var of string
  | Const of int
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Exp of t * t

val pp_ast : Format.formatter -> t -> unit
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
