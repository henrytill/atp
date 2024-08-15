module Syntax : sig
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
end

module Semantics : sig
  val simplify_with_count : Syntax.t -> Syntax.t * int
  val simplify : Syntax.t -> Syntax.t
end

val parse : Lexing.lexbuf -> Syntax.t option
val parse_string : string -> Syntax.t option
