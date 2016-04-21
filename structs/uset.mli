(* $Id: tree.ml,v 1.3 2004/08/17 20:02:02 ruml Exp $

   dense unordered sets.  constant time membership query, insertion,
   and removal.  Iterating over members only takes time proportional
   to number of members.

   stores non-negative ints.  size fixed at creation.
*)


type t

  
val make : int -> t
  (** size fixed at creation. *)
  
exception Present
  
val insert : t -> int -> unit
  (** raises Present  if already present *)
  
val remove : t -> int -> unit
  (** raises Not_found if not already present *)
  
val mem : t -> int -> bool

val max : t -> int
  (** highest index that can be present in set *)

val count : t -> int
  (** number of items currently present *)

exception Empty
  
val random : t -> int
  (** raises Empty if no elements *)
  
val iter : (int -> unit) -> t -> unit
  (** [f] can safely modify [s] during traverse *)
  
val iter_unsafe : (int -> 'a) -> t -> unit


(* EOF *)
