module Internal : sig
  val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
  val onatoms : ('b -> 'b Syntax.Formula.t) -> 'b Syntax.Formula.t -> 'b Syntax.Formula.t
  val overatoms : ('a -> 'b -> 'b) -> 'a Syntax.Formula.t -> 'b -> 'b
  val setify : 'a list -> 'a list
  val atom_union : ('a -> 'b list) -> 'a Syntax.Formula.t -> 'b list
  val atoms : 'a Syntax.Formula.t -> 'a list

  val itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  (** Alias for [List.fold_right]. *)

  val onallvaluations : (('a -> bool) -> bool) -> ('a -> bool) -> 'a list -> bool
  (** Returns [true] on all possible valuations of the atoms [ats], using an existing [v] for all other atoms. *)

  val print_truthtable : Format.formatter -> Syntax.t -> unit
end

val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
(** [eval fm v] returns the truth-value of [fm] using the valuation [v]. *)

val atoms : 'a Syntax.Formula.t -> 'a list
(** [atoms fm] returns a list of all the atoms in [fm]. *)

val print_truthtable : Format.formatter -> Syntax.t -> unit
(** [print_truthtable fmt fm] prints a truthtable for [fm] using [fmt]. *)