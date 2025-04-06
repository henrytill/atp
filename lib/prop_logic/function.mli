include module type of Function_intf
(** @inline *)

(** Functor to create a Patricia tree implementation of finite partial functions for a given key
    type.

    The key type must provide comparison and hash functions. *)
module Make (Key : KEY_TYPE) : S with type key = Key.t
