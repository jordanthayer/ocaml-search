(* $Id: cache.mli,v 1.2 2003/09/27 01:11:43 ruml Exp ruml $

   fixed-size LRU cache.  Maps keys to values like a Hashtbl, but can
   only remember a limited number of pairs.  Forgets the least
   recently accessed.
*)


type ('a, 'b) t

val create : ?eq:('a -> 'a -> bool) -> int -> ('a, 'b) t

val insert : ('a, 'b) t -> 'a -> 'b -> unit

val find : ('a, 'b) t -> 'a -> 'b

val mem : ('a, 'b) t -> 'a -> bool

val find_or_compute : ('a, 'b) t -> ('a -> 'b) -> 'a -> 'b

val memoize : ('a -> 'b) -> int -> 'a -> 'b
  (** only works on functions of a single argument
      the first bit is the function to be memoized
      the int is cache size *)

val to_list : ('a, 'b) t -> ('a * 'b) list


(** for debugging ***)


val test1 : unit -> unit


(* EOF *)
