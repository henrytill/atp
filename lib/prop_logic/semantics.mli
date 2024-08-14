val eval : Syntax.t -> (Syntax.Prop.t -> bool) -> bool
val atoms : 'a Syntax.Formula.t -> 'a list
val print_truthtable : Format.formatter -> Syntax.Prop.t Syntax.Formula.t -> unit
