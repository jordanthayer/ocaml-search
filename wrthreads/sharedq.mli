(* $Id: sharedq.mli,v 1.1 2003/06/20 16:31:46 ruml Exp $
   
   FIFO queues that have built-in mutex checking, so they can be
   called from multiple threads.  Destructive.

   Note that all code linked with this must be compiled with -thread
   and that the threads library must be linked in too.
*)


type 'a t

exception Empty

val create : unit -> 'a t

val add : 'a t -> 'a -> unit
  (** goes to rear *)

val take : 'a t -> 'a
  (** take from front.  can raise Empty *)

val take_next : 'a t -> 'a
  (** take from front.  blocks until there is something to take. *)

val is_empty : 'a t -> bool

val iter : ('a -> unit) -> 'a t -> unit

val fold : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
  (** note that the q is protected against other simultaneous calls for the
    entire duration of the iter *)


(* EOF *)
