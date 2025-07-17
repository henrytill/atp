(** Implementation of finite partial functions using Patricia trees. *)

module type KEY_TYPE = sig
  (** Required signature for key types used in finite partial functions. *)

  type t
  (** The type of keys. *)

  val compare : t -> t -> int
  (** [compare x y] returns a negative integer if [x] is less than [y], 0 if [x] is equal to [y],
      and a positive integer if [x] is greater than [y]. *)

  val hash : t -> int
  (** [hash x] returns a non-negative integer hash value for [x]. *)
end

module type S = sig
  (** Signature for finite partial functions. *)

  type key
  (** The type of keys in the partial function. *)

  type 'a t
  (** The type of partial functions from keys to values of type ['a]. *)

  val undefined : 'a t
  (** The empty partial function. *)

  val is_undefined : 'a t -> bool
  (** [is_undefined f] tests if [f] is the empty partial function. *)

  val applyd : 'a t -> (key -> 'a) -> key -> 'a
  (** [applyd f default k] applies function [f] to key [k].

      If [k] is not mapped in [f], returns [default k]. *)

  val tryapplyd : 'a t -> 'a -> key -> 'a
  (** [tryapplyd f default k] applies function [f] to key [k].

      If [k] is not mapped in [f], returns [default]. *)

  val apply : 'a t -> key -> 'a
  (** [apply f k] applies function [f] to key [k].

      Raises [Failure "apply"] if [k] is not mapped in [f]. *)

  val ( |-> ) : key -> 'a -> 'a t -> 'a t
  (** [k |-> v f] extends partial function [f] by mapping key [k] to value [v].

      If [k] was already mapped in [f], its value is updated to [v]. *)

  val ( |=> ) : key -> 'a -> 'a t
  (** [k |=> v] creates a new partial function mapping only key [k] to value [v].

      Equivalent to [k |-> v undefined]. *)
end
