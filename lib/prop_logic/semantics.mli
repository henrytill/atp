val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
(** [eval fm v] returns the truth-value of [fm] using the valuation [v]. *)

val onatoms : ('a -> 'b Syntax.Formula.t) -> 'a Syntax.Formula.t -> 'b Syntax.Formula.t
val overatoms : ('a -> 'b -> 'b) -> 'a Syntax.Formula.t -> 'b -> 'b
val setify : 'a list -> 'a list
val atom_union : ('a -> 'b list) -> 'a Syntax.Formula.t -> 'b list

val atoms : 'a Syntax.Formula.t -> 'a list
(** [atoms fm] returns a list of the atoms in [fm]. *)

val onallvaluations :
  (module Map.OrderedType with type t = 'a) -> (('a -> bool) -> 'b) -> 'a list -> 'b Seq.t

val print_truthtable : Format.formatter -> Syntax.t -> unit
(** [print_truthtable fmt fm] prints a truthtable for [fm] using [fmt]. *)

val tautology : Syntax.t -> bool
val unsatisfiable : Syntax.t -> bool
val satisfiable : Syntax.t -> bool

module Function : sig
  type (_, _) t

  val ( |-> ) : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val ( |=> ) : 'a -> 'b -> ('a, 'b) t
end

val psubst : ('a, 'a Syntax.Formula.t) Function.t -> 'a Syntax.Formula.t -> 'a Syntax.Formula.t
