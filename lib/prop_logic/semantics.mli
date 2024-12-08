(** Semantic operations for propositional logic formulas. *)

val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
(** [eval fm v] evaluates the truth value of formula [fm] under valuation [v].

    The valuation [v] maps propositional variables to boolean values. *)

val onatoms : ('a -> 'b Syntax.Formula.t) -> 'a Syntax.Formula.t -> 'b Syntax.Formula.t
(** [onatoms f fm] applies function [f] to every atomic formula in [fm], preserving the structure.

    This function traverses the formula and transforms each atom while maintaining the logical
    connectives. *)

val overatoms : ('a -> 'b -> 'b) -> 'a Syntax.Formula.t -> 'b -> 'b
(** [overatoms f fm acc] folds function [f] over all atoms in [fm] with accumulator [acc].

    This function traverses the formula and accumulates a result by applying [f] to each atom. *)

val atom_union : ('a -> Syntax.Prop.t list) -> 'a Syntax.Formula.t -> Syntax.Prop.t list
(** [atom_union f fm] collects and combines lists of atomic propositions from [fm] using [f].

    The resulting list is sorted and contains no duplicates. *)

val atoms : Syntax.t -> Syntax.Prop.t list
(** [atoms fm] returns a sorted list of all unique atomic propositions appearing in [fm]. *)

val onallvaluations : ((Syntax.Prop.t -> bool) -> 'a) -> Syntax.Prop.t list -> 'a Seq.t
(** [onallvaluations f props] generates a sequence by applying [f] to all possible truth valuations
    of the propositions in [props]. *)

val print_truthtable : Format.formatter -> Syntax.t -> unit
(** [print_truthtable fmt fm] prints a truth table for formula [fm] using formatter [fmt].

    The table includes all atomic propositions in the formula and their possible combinations of
    truth values, along with the formula's truth value in each case. *)

val tautology : Syntax.t -> bool
(** [tautology fm] tests if [fm] is a tautology (true under all valuations). Returns [true] if [fm]
    is a tautology, [false] otherwise. *)

val unsatisfiable : Syntax.t -> bool
(** [unsatisfiable fm] tests if [fm] is unsatisfiable (false under all valuations). Returns [true]
    if [fm] is unsatisfiable, [false] otherwise. *)

val satisfiable : Syntax.t -> bool
(** [satisfiable fm] tests if [fm] is satisfiable (true under some valuation). Returns [true] if
    [fm] is satisfiable, [false] otherwise. *)

module Function : sig
  (** Implementation of finite partial functions using Patricia trees.

      Maps propositional variables to values using a compressed binary trie data structure. *)

  type 'a t
  (** The type of partial functions from propositions to values of type ['a]. *)

  val undefined : 'a t
  (** The empty partial function. *)

  val is_undefined : 'a t -> bool
  (** [is_undefined f] tests if [f] is the empty partial function. *)

  val ( |-> ) : Syntax.Prop.t -> 'a -> 'a t -> 'a t
  (** [p |-> v f] extends partial function [f] by mapping proposition [p] to value [v].

      If [p] was already mapped in [f], its value is updated to [v]. *)

  val ( |=> ) : Syntax.Prop.t -> 'a -> 'a t
  (** [p |=> v] creates a new partial function mapping only proposition [p] to value [v].

      Equivalent to [p |-> v undefined]. *)
end

val psubst : Syntax.t Function.t -> Syntax.t -> Syntax.t
(** [psubst subfn fm] performs simultaneous substitution in formula [fm] using [subfn].

    For each atomic proposition [p] in [fm], if [subfn] maps [p] to some formula, that formula is
    substituted for [p]. If [subfn] is undefined at [p], [p] remains unchanged. *)
