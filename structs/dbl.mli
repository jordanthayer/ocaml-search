(* $Id: dbl.mli,v 1.1 2003/06/24 17:58:08 ruml Exp ruml $

   doubly-linked lists.  You probably want to use the Dblq interface, which
   keeps track of the head and tail.

   can add or remove at any point
   Updates are destructive.
*)



type 'a t

type 'a cell

val nil : 'a t
  (** the empty list *)

val is_nil : 'a t -> bool

val cons : 'a -> 'a t -> 'a t -> 'a t


exception Nil

val in_list : 'a t -> bool

val data : 'a t -> 'a
  (** can raise nil *)

val next : 'a t -> 'a t

val prev : 'a t -> 'a t


val add_before : 'a -> 'a t -> 'a t
  (** returns new cons *)

val add_after : 'a -> 'a t -> 'a t
  (** returns new cons *)


val unlink : 'a t -> unit
  (** sets neighboring cells to point around this one.  This cell's
    links are not modified *)


val length_forward : 'a t -> int


val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** front to rear *)

val fold_right : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** rear to front *)


val iter_left : ('a -> unit) -> 'a t -> unit

val iter_right : ('a -> unit) -> 'a t -> unit


val find : ('a -> bool) -> 'a t -> 'a
  (** can raise Not_found *)

val exists : ('a -> bool) -> 'a t -> bool



(* EOF *)
