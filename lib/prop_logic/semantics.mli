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

val dual : 'a Syntax.Formula.t -> 'a Syntax.Formula.t
(** [dual fm] exchanges [And] with [Or] and [True] with [False] in [fm] *)

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

module Function : Function_intf.S with type key = Syntax.Prop.t
(** Access to the underlying Function module implementation. *)

val psubst : Syntax.t Function.t -> Syntax.t -> Syntax.t
(** [psubst subfn fm] performs simultaneous substitution in formula [fm] using [subfn].

    For each atomic proposition [p] in [fm], if [subfn] maps [p] to some formula, that formula is
    substituted for [p]. If [subfn] is undefined at [p], [p] remains unchanged. *)
