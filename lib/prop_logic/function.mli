module type KEY_TYPE = sig
  type t

  val compare : t -> t -> int
  val hash : t -> int
end

module type S = sig
  type key
  type 'a t

  val undefined : 'a t
  val is_undefined : 'a t -> bool
  val applyd : 'a t -> (key -> 'a) -> key -> 'a
  val tryapplyd : 'a t -> key -> 'a -> 'a
  val apply : 'a t -> key -> 'a
  val ( |-> ) : key -> 'a -> 'a t -> 'a t
  val ( |=> ) : key -> 'a -> 'a t
end

module Make (Key : KEY_TYPE) : S with type key = Key.t
