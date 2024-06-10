module Prop : sig
  type t

  val inj : string -> t
  val prj : t -> string
  val pp_ast : Format.formatter -> t -> unit
  val equal : t -> t -> bool
end

module Formula : sig
  type 'a t =
    | Atom of 'a
    | False
    | True
    | Not of 'a t
    | And of 'a t * 'a t
    | Or of 'a t * 'a t
    | Imp of 'a t * 'a t
    | Iff of 'a t * 'a t
    | Forall of string * 'a t
    | Exists of string * 'a t

  val pp_ast : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
end

type t = Prop.t Formula.t

val pp_ast : Format.formatter -> t -> unit
val pp : Format.formatter -> t -> unit
val equal : t -> t -> bool
