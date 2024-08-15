module Syntax = Intro__syntax

val simplify_with_count : Syntax.t -> Syntax.t * int
val simplify : Syntax.t -> Syntax.t
