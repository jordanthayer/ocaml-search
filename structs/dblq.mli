(* $Id: dblq.mli,v 1.3 2003/09/27 01:12:01 ruml Exp ruml $

   double-ended queues

   Elements can be added or removed at front or rear.
   Updates are destructive.
   An additional unsafe interface allows removing from arbitrary locations.
*)



type 'a cell

type 'a t

exception Empty


val create : unit -> 'a t

val copy : 'a t -> 'a t

val clear : 'a t -> unit


val push_front : 'a t -> 'a -> unit

val push_rear : 'a t -> 'a -> unit


val peek_front : 'a t -> 'a

val pop_front : 'a t -> 'a

val peek_rear : 'a t -> 'a

val pop_rear : 'a t -> 'a

val data : 'a cell -> 'a

val in_q : 'a cell -> bool


val is_empty : 'a t -> bool

val length : 'a t -> int


val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** front to rear *)

val fold_right : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** rear to front *)


val iter_left : ('a -> unit) -> 'a t -> unit
  (** front to rear *)

val iter_right : ('a -> unit) -> 'a t -> unit
  (** rear to front *)


val find : ('a -> bool) -> 'a t -> 'a
  (** can raise Not_found *)

val exists : ('a -> bool) -> 'a t -> bool


val partition : ('a -> bool) -> 'a t -> ('a t * 'a t)
  (** returns satisfying and unsatisfying *)

val filter : ('a -> bool) -> 'a t -> 'a t

val matching : ('a -> bool) -> 'a t -> 'a list
  (** like filter but returns a list *)

val append : 'a t -> 'a t -> 'a t
  (** returns completely fresh Dblq *)


val to_list : 'a t -> 'a list


(*********** dangerous interface *************)


val push_front_cons : 'a t -> 'a -> 'a cell
  (** returns the new cons cell used in the q *)


val remove : 'a t -> 'a cell -> 'a
  (** given a dblq and a cons cell that is in it, removes the cell and
    returns the data.  can corrupt the q that the cell is part of if
    that q is not passed as the first argument.  *)

val replace : 'a t -> 'a cell -> 'a -> 'a cell


(* EOF *)
