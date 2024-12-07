val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
(** [eval fm v] returns the truth-value of [fm] using the valuation [v]. *)

val onatoms : ('a -> 'b Syntax.Formula.t) -> 'a Syntax.Formula.t -> 'b Syntax.Formula.t
val overatoms : ('a -> 'b -> 'b) -> 'a Syntax.Formula.t -> 'b -> 'b

module Prop_operations : sig
  val atom_union : (Syntax.Prop.t -> Syntax.Prop.t list) -> Syntax.t -> Syntax.Prop.t list

  val atoms : Syntax.t -> Syntax.Prop.t list
  (** [atoms fm] returns a list of the atoms in [fm]. *)

  val onallvaluations : ((Syntax.Prop.t -> bool) -> 'a) -> Syntax.Prop.t list -> 'a Seq.t
end

val print_truthtable : Format.formatter -> Syntax.t -> unit
(** [print_truthtable fmt fm] prints a truthtable for [fm] using [fmt]. *)

val tautology : Syntax.t -> bool
val unsatisfiable : Syntax.t -> bool
val satisfiable : Syntax.t -> bool

module Prop_function : sig
  type 'a t

  val ( |-> ) : Syntax.Prop.t -> 'a -> 'a t -> 'a t
  val ( |=> ) : Syntax.Prop.t -> 'a -> 'a t
end

val psubst : Syntax.t Prop_function.t -> Syntax.t -> Syntax.t
