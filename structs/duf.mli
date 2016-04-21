(* $Id: duf.mli,v 1.1 2006/06/30 04:34:52 ruml Exp $

   destructive union find
*)


type 'a t
  
exception Not_new

  
val make : int -> 'a t

  
val add : 'a t -> 'a -> unit
  (** adds if not already a member *)

val mem : 'a t -> 'a -> bool
  
val add_new : 'a t -> 'a -> unit
  (** can raise Not_new *)

  
val join : 'a t -> 'a -> 'a -> unit
  (** can raise Not_found *)

val add_join : 'a t -> 'a -> 'a -> unit
  (** adds [a] and joins it with [b].  [b] is assumed to already be a
    member. *)

val same_p : 'a t -> 'a -> 'a -> bool

val rep : 'a t -> 'a -> 'a
  (** returns the current canonical representative of the argument *)


val iter : ('a -> unit) -> 'a t -> unit

val to_lists : 'a t -> 'a list list
  (** list of lists of equivalent objects *)

val of_list : 'a list -> 'a t
  (** every object starts separate *)
  
val of_lists : 'a list list -> 'a t


(* EOF *)
