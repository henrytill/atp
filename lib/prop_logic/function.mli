include module type of Function_intf
(** @inline *)

module Make (Key : KEY_TYPE) : S with type key = Key.t
