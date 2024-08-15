module Syntax = Prop_logic__syntax
module Prop = Prop_logic__syntax.Prop
module Formula = Prop_logic__syntax.Formula

module Internal : sig
  val eval : Syntax.t -> (Prop.t -> bool) -> bool
  val onatoms : ('b -> 'b Formula.t) -> 'b Formula.t -> 'b Formula.t
  val overatoms : ('a -> 'b -> 'b) -> 'a Formula.t -> 'b -> 'b
  val setify : 'a list -> 'a list
  val atom_union : ('a -> 'b list) -> 'a Formula.t -> 'b list
  val atoms : 'a Formula.t -> 'a list
  val itlist : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
  val onallvaluations : (('a -> bool) -> bool) -> ('a -> bool) -> 'a list -> bool
  val print_truthtable : Format.formatter -> Prop.t Formula.t -> unit
end

val eval : Syntax.t -> (Prop.t -> bool) -> bool
val atoms : 'a Formula.t -> 'a list
val print_truthtable : Format.formatter -> Prop.t Formula.t -> unit
