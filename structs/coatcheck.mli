(* $Id: coatcheck.mli,v 1.1 2005/04/03 19:17:46 ruml Exp ruml $

   coatcheck - mapping objects to and from non-negative integers

   The Strtab module is a version of this optimized for strings.
*)


type 'a t


val create : int -> 'a t

val copy : 'a t -> 'a t

val intern : 'a t -> 'a -> int
(** returns a unique and repeatable integer 'coatcheck tag' corresponding
    to the argument *)

val retrieve : 'a t -> int -> 'a
(** given an integer 'coatcheck tag', returns the corresponding object.
    Does not actually remove the object from the coatcheck. *)

val count : 'a t -> int
(** number of items currently interned *)

val iter : (int -> 'a -> unit) -> 'a t -> unit
(** calls [f] on tag and object.  Probably best not to rely on the
    order. *)

val fold : ('a -> int -> 'b -> 'a) -> 'a -> 'b t -> 'a
(** again, probably best not to rely on the order. *)


module Str : sig
  (** A special Coatcheck that is optimized for checking strings. *)

  type t

  val create : int -> t

  val length : t -> int
  (** Get the current number of mappings. *)

  val insert : t -> string -> unit
  (** Add a new string. *)

  val intern : t -> string -> string
  (** Gets (creates if necessary) the unique in-memory copy of the given
      string. *)

  val id : t -> string -> int
  (** Gets (creates if necessary) the unique ID associated with the
      given string. *)

  val lookup : t -> int -> string
  (** Lookup the interned string for the given ID. *)

  val iter : (int -> string -> unit) -> t -> unit

  val fold : ('a -> int -> string -> 'a) -> 'a -> t -> 'a
end

(* EOF *)
